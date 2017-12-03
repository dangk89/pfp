#include <stdint.h>

/*
 * Initialisation
*/

struct futhark_context_config ;
struct futhark_context_config *futhark_context_config_new();
void futhark_context_config_free(struct futhark_context_config *cfg);
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag);
struct futhark_context ;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);
void futhark_context_free(struct futhark_context *ctx);
int futhark_context_sync(struct futhark_context *ctx);

/*
 * Arrays
*/

struct futhark_i32_2d ;
struct futhark_i32_2d *futhark_new_i32_2d(struct futhark_context *ctx,
                                          int32_t *data, int dim0, int dim1);
int futhark_free_i32_2d(struct futhark_context *ctx,
                        struct futhark_i32_2d *arr);
int futhark_values_i32_2d(struct futhark_context *ctx,
                          struct futhark_i32_2d *arr, int32_t *data);
int64_t *futhark_shape_i32_2d(struct futhark_context *ctx,
                              struct futhark_i32_2d *arr);

/*
 * Opaque values
*/


/*
 * Entry points
*/

int futhark_main(struct futhark_context *ctx, struct futhark_i32_2d **out0);

/*
 * Miscellaneous
*/

void futhark_debugging_report(struct futhark_context *ctx);
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
/* Crash and burn. */

#include <stdarg.h>

static const char *fut_progname;

static void panic(int eval, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
        fprintf(stderr, "%s: ", fut_progname);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
        exit(eval);
}

/* Some simple utilities for wall-clock timing.

   The function get_wall_time() returns the wall time in microseconds
   (with an unspecified offset).
*/

#ifdef _WIN32

#include <windows.h>

static int64_t get_wall_time() {
  LARGE_INTEGER time,freq;
  assert(QueryPerformanceFrequency(&freq));
  assert(QueryPerformanceCounter(&time));
  return ((double)time.QuadPart / freq.QuadPart) * 1000000;
}

#else
/* Assuming POSIX */

#include <time.h>
#include <sys/time.h>

static int64_t get_wall_time() {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

#endif

#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <errno.h>
#include <ctype.h>
#include <errno.h>
#include <getopt.h>
//// Text I/O

struct array_reader {
  char* elems;
  int64_t n_elems_space;
  int64_t elem_size;
  int64_t n_elems_used;
  int64_t *shape;
  int (*elem_reader)(void*);
};

static int peekc() {
  int c = getchar();
  if (c != EOF) {
    ungetc(c,stdin);
  }
  return c;
}

static int next_is_not_constituent() {
  int c = peekc();
  return c == EOF || !isalnum(c);
}

static void skipspaces() {
  int c = getchar();
  if (isspace(c)) {
    skipspaces();
  } else if (c == '-' && peekc() == '-') {
    // Skip to end of line.
    for (; c != '\n' && c != EOF; c = getchar());
    // Next line may have more spaces.
    skipspaces();
  } else if (c != EOF) {
    ungetc(c, stdin);
  }
}

static int read_str_elem(struct array_reader *reader) {
  int ret;
  if (reader->n_elems_used == reader->n_elems_space) {
    reader->n_elems_space *= 2;
    reader->elems = (char*) realloc(reader->elems,
                                    reader->n_elems_space * reader->elem_size);
  }

  ret = reader->elem_reader(reader->elems + reader->n_elems_used * reader->elem_size);

  if (ret == 0) {
    reader->n_elems_used++;
  }

  return ret;
}

static int read_str_array_elems(struct array_reader *reader, int dims) {
  int c;
  int ret;
  int first = 1;
  char *knows_dimsize = (char*) calloc(dims,sizeof(char));
  int cur_dim = dims-1;
  int64_t *elems_read_in_dim = (int64_t*) calloc(dims,sizeof(int64_t));
  while (1) {
    skipspaces();

    c = getchar();
    if (c == ']') {
      if (knows_dimsize[cur_dim]) {
        if (reader->shape[cur_dim] != elems_read_in_dim[cur_dim]) {
          ret = 1;
          break;
        }
      } else {
        knows_dimsize[cur_dim] = 1;
        reader->shape[cur_dim] = elems_read_in_dim[cur_dim];
      }
      if (cur_dim == 0) {
        ret = 0;
        break;
      } else {
        cur_dim--;
        elems_read_in_dim[cur_dim]++;
      }
    } else if (c == ',') {
      skipspaces();
      c = getchar();
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        first = 1;
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else if (cur_dim == dims - 1) {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
      } else {
        ret = 1;
        break;
      }
    } else if (c == EOF) {
      ret = 1;
      break;
    } else if (first) {
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
        first = 0;
      }
    } else {
      ret = 1;
      break;
    }
  }

  free(knows_dimsize);
  free(elems_read_in_dim);
  return ret;
}

static int read_str_empty_array(const char *type_name, int64_t *shape, int64_t dims) {
  char c;
  if (scanf("empty") == EOF) {
    return 1;
  }

  c = getchar();
  if (c != '(') {
    return 1;
  }

  for (int i = 0; i < dims-1; i++) {
    c = getchar();
    if (c != '[') {
      return 1;
    }
    c = getchar();
    if (c != ']') {
      return 1;
    }
  }

  int n = strlen(type_name);
  for (int i = 0; i < n; i++) {
    c = getchar();
    if (c != type_name[i]) {
      return 1;
    }
  }

  if (getchar() != ')') {
    return 1;
  }

  for (int i = 0; i < dims; i++) {
    shape[i] = 0;
  }

  return 0;
}

static int read_str_array(int64_t elem_size, int (*elem_reader)(void*),
                          const char *type_name,
                          void **data, int64_t *shape, int64_t dims) {
  int ret;
  struct array_reader reader;
  int64_t read_dims = 0;

  while (1) {
    int c;
    skipspaces();
    c = getchar();
    if (c=='[') {
      read_dims++;
    } else {
      if (c != EOF) {
        ungetc(c, stdin);
      }
      break;
    }
  }

  if (read_dims == 0) {
    return read_str_empty_array(type_name, shape, dims);
  }

  if (read_dims != dims) {
    return 1;
  }

  reader.shape = shape;
  reader.n_elems_used = 0;
  reader.elem_size = elem_size;
  reader.n_elems_space = 16;
  reader.elems = (char*) realloc(*data, elem_size*reader.n_elems_space);
  reader.elem_reader = elem_reader;

  ret = read_str_array_elems(&reader, dims);

  *data = reader.elems;

  return ret;
}

/* Makes a copy of numeric literal removing any underscores, and
   length of the literal. */
static int remove_underscores(char* buf) {
  int buf_index = 0;
  char c = getchar();
  while (isxdigit(c) || c == '.' || c == '+' || c == '-' ||
         c == 'x' || c == 'X' ||
         c == 'p' || c == 'P' || /* exponent for hex. floats */
         c == 'e' || c == 'E' || c == '_') {
    if (c == '_') {
      c = getchar();
      continue;
    }
    else {
      buf[buf_index++] = c;
      c = getchar();
    }
  }
  buf[buf_index] = 0;
  ungetc(c, stdin);             /* unget 'i' */
  return buf_index;
}

static int read_str_i8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNi8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(int8_t*)dest = x;
    scanf("i8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNu8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(uint8_t*)dest = x;
    scanf("u8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("i16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    printf("fail\n");
    return 1;
  }
}

static int read_str_u16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("u16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("i32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("u32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi64, (int64_t*)dest) == 1) {
    scanf("i64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  // FIXME: This is not correct, as SCNu64 only permits decimal
  // literals.  However, SCNi64 does not handle very large numbers
  // correctly (it's really for signed numbers, so that's fair).
  if (sscanf(buf, "%"SCNu64, (uint64_t*)dest) == 1) {
    scanf("u64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%f", (float*)dest) == 1) {
    scanf("f32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%lf", (double*)dest) == 1) {
    scanf("f64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_bool(void* dest) {
  /* This is a monstrous hack.  Maybe we should get a proper lexer in here. */
  char b[4];
  skipspaces();
  if (scanf("%4c", b) == 1) {
    if (strncmp(b, "true", 4) == 0) {
      *(char*)dest = 1;
      return 0;
    } else if (strncmp(b, "fals", 4) == 0 && getchar() == 'e') {
      *(char*)dest = 0;
      return 0;
    } else {
      return 1;
    }
  } else {
    return 1;
  }
}

static int write_str_i8(FILE *out, int8_t *src) {
  return fprintf(out, "%hhdi8", *src);
}

static int write_str_u8(FILE *out, uint8_t *src) {
  return fprintf(out, "%hhuu8", *src);
}

static int write_str_i16(FILE *out, int16_t *src) {
  return fprintf(out, "%hdi16", *src);
}

static int write_str_u16(FILE *out, uint16_t *src) {
  return fprintf(out, "%huu16", *src);
}

static int write_str_i32(FILE *out, int32_t *src) {
  return fprintf(out, "%di32", *src);
}

static int write_str_u32(FILE *out, uint32_t *src) {
  return fprintf(out, "%uu32", *src);
}

static int write_str_i64(FILE *out, int64_t *src) {
  return fprintf(out, "%"PRIi64"i64", *src);
}

static int write_str_u64(FILE *out, uint64_t *src) {
  return fprintf(out, "%"PRIu64"u64", *src);
}

static int write_str_f32(FILE *out, float *src) {
  return fprintf(out, "%.6ff32", *src);
}

static int write_str_f64(FILE *out, double *src) {
  return fprintf(out, "%.6ff64", *src);
}

static int write_str_bool(FILE *out, void *src) {
  return fprintf(out, *(char*)src ? "true" : "false");
}

//// Binary I/O

#define BINARY_FORMAT_VERSION 2
#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

// Reading little-endian byte sequences.  On big-endian hosts, we flip
// the resulting bytes.

static int read_byte(void* dest) {
  int num_elems_read = fread(dest, 1, 1, stdin);
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_2byte(void* dest) {
  uint16_t x;
  int num_elems_read = fread(&x, 2, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  *(uint16_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_4byte(void* dest) {
  uint32_t x;
  int num_elems_read = fread(&x, 4, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  *(uint32_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_8byte(void* dest) {
  uint64_t x;
  int num_elems_read = fread(&x, 8, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  *(uint64_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int write_byte(void* dest) {
  int num_elems_written = fwrite(dest, 1, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_2byte(void* dest) {
  uint16_t x = *(uint16_t*)dest;
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  int num_elems_written = fwrite(&x, 2, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_4byte(void* dest) {
  uint32_t x = *(uint32_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  int num_elems_written = fwrite(&x, 4, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_8byte(void* dest) {
  uint64_t x = *(uint64_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  int num_elems_written = fwrite(&x, 8, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

//// Types

typedef int (*writer)(FILE*, void*);
typedef int (*reader)(void*);

struct primtype_info_t {
  const char binname[4]; // Used for parsing binary data.
  const char* type_name; // Same name as in Futhark.
  const int size; // in bytes
  const writer write_str; // Write in text format.
  const reader read_str; // Read in text format.
  const writer write_bin; // Write in binary format.
  const reader read_bin; // Read in binary format.
};

static const struct primtype_info_t i8 =
  {.binname = "  i8", .type_name = "i8",   .size = 1,
   .write_str = (writer)write_str_i8, .read_str = (reader)read_str_i8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
static const struct primtype_info_t i16 =
  {.binname = " i16", .type_name = "i16",  .size = 2,
   .write_str = (writer)write_str_i16, .read_str = (reader)read_str_i16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
static const struct primtype_info_t i32 =
  {.binname = " i32", .type_name = "i32",  .size = 4,
   .write_str = (writer)write_str_i32, .read_str = (reader)read_str_i32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
static const struct primtype_info_t i64 =
  {.binname = " i64", .type_name = "i64",  .size = 8,
   .write_str = (writer)write_str_i64, .read_str = (reader)read_str_i64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
static const struct primtype_info_t u8 =
  {.binname = "  u8", .type_name = "u8",   .size = 1,
   .write_str = (writer)write_str_u8, .read_str = (reader)read_str_u8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
static const struct primtype_info_t u16 =
  {.binname = " u16", .type_name = "u16",  .size = 2,
   .write_str = (writer)write_str_u16, .read_str = (reader)read_str_u16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
static const struct primtype_info_t u32 =
  {.binname = " u32", .type_name = "u32",  .size = 4,
   .write_str = (writer)write_str_u32, .read_str = (reader)read_str_u32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
static const struct primtype_info_t u64 =
  {.binname = " u64", .type_name = "u64",  .size = 8,
   .write_str = (writer)write_str_u64, .read_str = (reader)read_str_u64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
static const struct primtype_info_t f32 =
  {.binname = " f32", .type_name = "f32",  .size = 4,
   .write_str = (writer)write_str_f32, .read_str = (reader)read_str_f32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
static const struct primtype_info_t f64 =
  {.binname = " f64", .type_name = "f64",  .size = 8,
   .write_str = (writer)write_str_f64, .read_str = (reader)read_str_f64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
static const struct primtype_info_t bool =
  {.binname = "bool", .type_name = "bool", .size = 1,
   .write_str = (writer)write_str_bool, .read_str = (reader)read_str_bool,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};

static const struct primtype_info_t* primtypes[] = {
  &i8, &i16, &i32, &i64,
  &u8, &u16, &u32, &u64,
  &f32, &f64,
  &bool,
  NULL // NULL-terminated
};

// General value interface.  All endian business taken care of at
// lower layers.

static int read_is_binary() {
  skipspaces();
  int c = getchar();
  if (c == 'b') {
    int8_t bin_version;
    int ret = read_byte(&bin_version);

    if (ret != 0) { panic(1, "binary-input: could not read version.\n"); }

    if (bin_version != BINARY_FORMAT_VERSION) {
      panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
            bin_version, BINARY_FORMAT_VERSION);
    }

    return 1;
  }
  ungetc(c, stdin);
  return 0;
}

static const struct primtype_info_t* read_bin_read_type_enum() {
  char read_binname[4];

  int num_matched = scanf("%4c", read_binname);
  if (num_matched != 1) { panic(1, "binary-input: Couldn't read element type.\n"); }

  const struct primtype_info_t **type = primtypes;

  for (; *type != NULL; type++) {
    // I compare the 4 characters manually instead of using strncmp because
    // this allows any value to be used, also NULL bytes
    if (memcmp(read_binname, (*type)->binname, 4) == 0) {
      return *type;
    }
  }
  panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname);
  return NULL;
}

static void read_bin_ensure_scalar(const struct primtype_info_t *expected_type) {
  int8_t bin_dims;
  int ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != 0) {
    panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
          bin_dims);
  }

  const struct primtype_info_t *bin_type = read_bin_read_type_enum();
  if (bin_type != expected_type) {
    panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
          expected_type->type_name,
          bin_type->type_name);
  }
}

//// High-level interface

static int read_bin_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  int ret;

  int8_t bin_dims;
  ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != dims) {
    panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
          dims, bin_dims);
  }

  const struct primtype_info_t *bin_primtype = read_bin_read_type_enum();
  if (expected_type != bin_primtype) {
    panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
          dims, expected_type->type_name, dims, bin_primtype->type_name);
  }

  uint64_t elem_count = 1;
  for (int i=0; i<dims; i++) {
    uint64_t bin_shape;
    ret = read_le_8byte(&bin_shape);
    if (ret != 0) { panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i); }
    elem_count *= bin_shape;
    shape[i] = (int64_t) bin_shape;
  }

  size_t elem_size = expected_type->size;
  void* tmp = realloc(*data, elem_count * elem_size);
  if (tmp == NULL) {
    panic(1, "binary-input: Failed to allocate array of size %i.\n",
          elem_count * elem_size);
  }
  *data = tmp;

  size_t num_elems_read = fread(*data, elem_size, elem_count, stdin);
  if (num_elems_read != elem_count) {
    panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
          elem_count, num_elems_read);
  }

  // If we're on big endian platform we must change all multibyte elements
  // from using little endian to big endian
  if (IS_BIG_ENDIAN && elem_size != 1) {
    char* elems = (char*) *data;
    for (uint64_t i=0; i<elem_count; i++) {
      char* elem = elems+(i*elem_size);
      for (unsigned int j=0; j<elem_size/2; j++) {
        char head = elem[j];
        int tail_index = elem_size-1-j;
        elem[j] = elem[tail_index];
        elem[tail_index] = head;
      }
    }
  }

  return 0;
}

static int read_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  if (!read_is_binary()) {
    return read_str_array(expected_type->size, (reader)expected_type->read_str, expected_type->type_name, data, shape, dims);
  } else {
    return read_bin_array(expected_type, data, shape, dims);
  }
}

static int write_str_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  if (rank==0) {
    elem_type->write_str(out, (void*)data);
  } else {
    int64_t len = shape[0];
    int64_t slice_size = 1;

    int64_t elem_size = elem_type->size;
    for (int64_t i = 1; i < rank; i++) {
      slice_size *= shape[i];
    }

    if (len*slice_size == 0) {
      printf("empty(");
      for (int64_t i = 1; i < rank; i++) {
        printf("[]");
      }
      printf("%s", elem_type->type_name);
      printf(")");
    } else if (rank==1) {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        elem_type->write_str(out, (void*) (data + i * elem_size));
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    } else {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        write_str_array(out, elem_type, data + i * slice_size * elem_size, shape+1, rank-1);
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    }
  }
  return 0;
}

static int write_bin_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  int64_t num_elems = 1;
  for (int64_t i = 0; i < rank; i++) {
    num_elems *= shape[i];
  }

  fputc('b', out);
  fputc((char)BINARY_FORMAT_VERSION, out);
  fwrite(&rank, sizeof(int8_t), 1, out);
  fputs(elem_type->binname, out);
  fwrite(shape, sizeof(int64_t), rank, out);

  if (IS_BIG_ENDIAN) {
    for (int64_t i = 0; i < num_elems; i++) {
      unsigned char *elem = data+i*elem_type->size;
      for (int64_t j = 0; j < elem_type->size; j++) {
        fwrite(&elem[elem_type->size-j], 1, 1, out);
      }
    }
  } else {
    fwrite(data, elem_type->size, num_elems, out);
  }

  return 0;
}

static int write_array(FILE *out, int write_binary,
                       const struct primtype_info_t *elem_type, void *data, int64_t *shape, int8_t rank) {
  if (write_binary) {
    return write_bin_array(out, elem_type, data, shape, rank);
  } else {
    return write_str_array(out, elem_type, data, shape, rank);
  }
}

static int read_scalar(const struct primtype_info_t *expected_type, void *dest) {
  if (!read_is_binary()) {
    return expected_type->read_str(dest);
  } else {
    read_bin_ensure_scalar(expected_type);
    return expected_type->read_bin(dest);
  }
}

static int write_scalar(FILE *out, int write_binary, const struct primtype_info_t *type, void *src) {
  if (write_binary) {
    return write_bin_array(out, type, src, NULL, 0);
  } else {
    return type->write_str(out, src);
  }
}

static int binary_output = 0;
static FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
static const char *entry_point = "main";
int parse_options(struct futhark_context_config *cfg, int argc,
                  char *const argv[])
{
    int ch;
    static struct option long_options[] = {{"write-runtime-to",
                                            required_argument, NULL, 1},
                                           {"runs", required_argument, NULL, 2},
                                           {"debugging", no_argument, NULL, 3},
                                           {"entry-point", required_argument,
                                            NULL, 4}, {"binary-output",
                                                       no_argument, NULL, 5},
                                           {0, 0, 0, 0}};
    
    while ((ch = getopt_long(argc, argv, ":t:r:De:b", long_options, NULL)) !=
           -1) {
        if (ch == 1 || ch == 't') {
            runtime_file = fopen(optarg, "w");
            if (runtime_file == NULL)
                panic(1, "Cannot open %s: %s\n", optarg, strerror(errno));
        }
        if (ch == 2 || ch == 'r') {
            num_runs = atoi(optarg);
            perform_warmup = 1;
            if (num_runs <= 0)
                panic(1, "Need a positive number of runs, not %s\n", optarg);
        }
        if (ch == 3 || ch == 'D')
            futhark_context_config_set_debugging(cfg, 1);
        if (ch == 4 || ch == 'e')
            entry_point = optarg;
        if (ch == 5 || ch == 'b')
            binary_output = 1;
        if (ch == ':')
            panic(-1, "Missing argument for option %s\n", argv[optind - 1]);
        if (ch == '?')
            panic(-1, "Unknown option %s\n", argv[optind - 1]);
    }
    return optind;
}
static void futrts_cli_entry_main(struct futhark_context *ctx)
{
    int64_t t_start, t_end;
    int time_runs;
    struct futhark_i32_2d *result_5133;
    
    if (perform_warmup) {
        time_runs = 0;
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_5133) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        assert(futhark_free_i32_2d(ctx, result_5133) == 0);
    }
    time_runs = 1;
    /* Proper run. */
    for (int run = 0; run < num_runs; run++) {
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_5133) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        if (run < num_runs - 1) {
            assert(futhark_free_i32_2d(ctx, result_5133) == 0);
        }
    }
    {
        int32_t *arr = calloc(sizeof(int32_t), futhark_shape_i32_2d(ctx,
                                                                    result_5133)[0] *
                              futhark_shape_i32_2d(ctx, result_5133)[1]);
        
        assert(arr != NULL);
        assert(futhark_values_i32_2d(ctx, result_5133, arr) == 0);
        write_array(stdout, binary_output, &i32, arr, futhark_shape_i32_2d(ctx,
                                                                           result_5133),
                    2);
        free(arr);
    }
    printf("\n");
    assert(futhark_free_i32_2d(ctx, result_5133) == 0);
}
typedef void entry_point_fun(struct futhark_context *);
struct entry_point_entry {
    const char *name;
    entry_point_fun *fun;
} ;
int main(int argc, char **argv)
{
    fut_progname = argv[0];
    
    struct entry_point_entry entry_points[] = {{.name ="main", .fun =
                                                futrts_cli_entry_main}};
    struct futhark_context_config *cfg = futhark_context_config_new();
    
    assert(cfg != NULL);
    
    int parsed_options = parse_options(cfg, argc, argv);
    
    argc -= parsed_options;
    argv += parsed_options;
    
    struct futhark_context *ctx = futhark_context_new(cfg);
    
    assert(ctx != NULL);
    
    int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
    entry_point_fun *entry_point_fun = NULL;
    
    for (int i = 0; i < num_entry_points; i++) {
        if (strcmp(entry_points[i].name, entry_point) == 0) {
            entry_point_fun = entry_points[i].fun;
            break;
        }
    }
    if (entry_point_fun == NULL) {
        fprintf(stderr,
                "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                entry_point);
        for (int i = 0; i < num_entry_points; i++)
            fprintf(stderr, "%s\n", entry_points[i].name);
        return 1;
    }
    entry_point_fun(ctx);
    if (runtime_file != NULL)
        fclose(runtime_file);
    futhark_debugging_report(ctx);
    futhark_context_free(ctx);
    futhark_context_config_free(cfg);
    return 0;
}
#ifdef _MSC_VER
#define inline __inline
#endif
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
static int32_t static_array_realtype_5128[2] = {58, 11};
static int32_t static_array_realtype_5129[2] = {20, 25};
static int32_t static_array_realtype_5130[2] = {2, 5};
static int32_t static_array_realtype_5131[2] = {3, 15};
struct memblock {
    int *references;
    char *mem;
    int64_t size;
} ;
struct futhark_context_config {
    int debugging;
} ;
struct futhark_context_config *futhark_context_config_new()
{
    struct futhark_context_config *cfg =
                                  malloc(sizeof(struct futhark_context_config));
    
    if (cfg == NULL)
        return NULL;
    cfg->debugging = 0;
    return cfg;
}
void futhark_context_config_free(struct futhark_context_config *cfg)
{
    free(cfg);
}
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int detail)
{
    cfg->debugging = detail;
}
struct futhark_context {
    int detail_memory;
    int debugging;
    int64_t peak_mem_usage_default;
    int64_t cur_mem_usage_default;
    struct memblock static_array_5117;
    struct memblock static_array_5118;
    struct memblock static_array_5119;
    struct memblock static_array_5120;
} ;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg)
{
    struct futhark_context *ctx = malloc(sizeof(struct futhark_context));
    
    if (ctx == NULL)
        return NULL;
    ctx->detail_memory = cfg->debugging;
    ctx->debugging = cfg->debugging;
    ctx->peak_mem_usage_default = 0;
    ctx->cur_mem_usage_default = 0;
    ctx->static_array_5117 = (struct memblock) {NULL,
                                                (char *) static_array_realtype_5128,
                                                0};
    ctx->static_array_5118 = (struct memblock) {NULL,
                                                (char *) static_array_realtype_5129,
                                                0};
    ctx->static_array_5119 = (struct memblock) {NULL,
                                                (char *) static_array_realtype_5130,
                                                0};
    ctx->static_array_5120 = (struct memblock) {NULL,
                                                (char *) static_array_realtype_5131,
                                                0};
    return ctx;
}
void futhark_context_free(struct futhark_context *ctx)
{
    free(ctx);
}
int futhark_context_sync(struct futhark_context *ctx)
{
    return 0;
}
static void memblock_unref(struct futhark_context *ctx, struct memblock *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (ctx->detail_memory)
            fprintf(stderr,
                    "Unreferencing block in default space: %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            ctx->cur_mem_usage_default -= block->size;
            free(block->mem);
            free(block->references);
            block->references = NULL;
            if (ctx->detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, ctx->cur_mem_usage_default);
        }
    }
}
static void memblock_alloc(struct futhark_context *ctx, struct memblock *block,
                           int32_t size)
{
    memblock_unref(ctx, block);
    block->mem = (char *) malloc(size);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    ctx->cur_mem_usage_default += size;
    if (ctx->detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in default space (now allocated: %ld bytes)",
                size, ctx->cur_mem_usage_default);
    if (ctx->cur_mem_usage_default > ctx->peak_mem_usage_default) {
        ctx->peak_mem_usage_default = ctx->cur_mem_usage_default;
        if (ctx->detail_memory)
            fprintf(stderr, " (new peak).\n");
    } else if (ctx->detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set(struct futhark_context *ctx, struct memblock *lhs,
                         struct memblock *rhs)
{
    memblock_unref(ctx, lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
void futhark_debugging_report(struct futhark_context *ctx)
{
    if (ctx->detail_memory) {
        fprintf(stderr, "Peak memory usage for default space: %ld bytes.\n",
                ctx->peak_mem_usage_default);
    }
    if (ctx->debugging) { }
}
struct futrts_int32_t_mem_int32_t_int32_t {
    int32_t v0;
    struct memblock v1;
    int32_t v2;
    int32_t v3;
} ;
static struct futrts_int32_t_mem_int32_t_int32_t
futrts_main(struct futhark_context *ctx);
static inline int8_t add8(int8_t x, int8_t y)
{
    return x + y;
}
static inline int16_t add16(int16_t x, int16_t y)
{
    return x + y;
}
static inline int32_t add32(int32_t x, int32_t y)
{
    return x + y;
}
static inline int64_t add64(int64_t x, int64_t y)
{
    return x + y;
}
static inline int8_t sub8(int8_t x, int8_t y)
{
    return x - y;
}
static inline int16_t sub16(int16_t x, int16_t y)
{
    return x - y;
}
static inline int32_t sub32(int32_t x, int32_t y)
{
    return x - y;
}
static inline int64_t sub64(int64_t x, int64_t y)
{
    return x - y;
}
static inline int8_t mul8(int8_t x, int8_t y)
{
    return x * y;
}
static inline int16_t mul16(int16_t x, int16_t y)
{
    return x * y;
}
static inline int32_t mul32(int32_t x, int32_t y)
{
    return x * y;
}
static inline int64_t mul64(int64_t x, int64_t y)
{
    return x * y;
}
static inline uint8_t udiv8(uint8_t x, uint8_t y)
{
    return x / y;
}
static inline uint16_t udiv16(uint16_t x, uint16_t y)
{
    return x / y;
}
static inline uint32_t udiv32(uint32_t x, uint32_t y)
{
    return x / y;
}
static inline uint64_t udiv64(uint64_t x, uint64_t y)
{
    return x / y;
}
static inline uint8_t umod8(uint8_t x, uint8_t y)
{
    return x % y;
}
static inline uint16_t umod16(uint16_t x, uint16_t y)
{
    return x % y;
}
static inline uint32_t umod32(uint32_t x, uint32_t y)
{
    return x % y;
}
static inline uint64_t umod64(uint64_t x, uint64_t y)
{
    return x % y;
}
static inline int8_t sdiv8(int8_t x, int8_t y)
{
    int8_t q = x / y;
    int8_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int16_t sdiv16(int16_t x, int16_t y)
{
    int16_t q = x / y;
    int16_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int32_t sdiv32(int32_t x, int32_t y)
{
    int32_t q = x / y;
    int32_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int64_t sdiv64(int64_t x, int64_t y)
{
    int64_t q = x / y;
    int64_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int8_t smod8(int8_t x, int8_t y)
{
    int8_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int16_t smod16(int16_t x, int16_t y)
{
    int16_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int32_t smod32(int32_t x, int32_t y)
{
    int32_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int64_t smod64(int64_t x, int64_t y)
{
    int64_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int8_t squot8(int8_t x, int8_t y)
{
    return x / y;
}
static inline int16_t squot16(int16_t x, int16_t y)
{
    return x / y;
}
static inline int32_t squot32(int32_t x, int32_t y)
{
    return x / y;
}
static inline int64_t squot64(int64_t x, int64_t y)
{
    return x / y;
}
static inline int8_t srem8(int8_t x, int8_t y)
{
    return x % y;
}
static inline int16_t srem16(int16_t x, int16_t y)
{
    return x % y;
}
static inline int32_t srem32(int32_t x, int32_t y)
{
    return x % y;
}
static inline int64_t srem64(int64_t x, int64_t y)
{
    return x % y;
}
static inline int8_t smin8(int8_t x, int8_t y)
{
    return x < y ? x : y;
}
static inline int16_t smin16(int16_t x, int16_t y)
{
    return x < y ? x : y;
}
static inline int32_t smin32(int32_t x, int32_t y)
{
    return x < y ? x : y;
}
static inline int64_t smin64(int64_t x, int64_t y)
{
    return x < y ? x : y;
}
static inline uint8_t umin8(uint8_t x, uint8_t y)
{
    return x < y ? x : y;
}
static inline uint16_t umin16(uint16_t x, uint16_t y)
{
    return x < y ? x : y;
}
static inline uint32_t umin32(uint32_t x, uint32_t y)
{
    return x < y ? x : y;
}
static inline uint64_t umin64(uint64_t x, uint64_t y)
{
    return x < y ? x : y;
}
static inline int8_t smax8(int8_t x, int8_t y)
{
    return x < y ? y : x;
}
static inline int16_t smax16(int16_t x, int16_t y)
{
    return x < y ? y : x;
}
static inline int32_t smax32(int32_t x, int32_t y)
{
    return x < y ? y : x;
}
static inline int64_t smax64(int64_t x, int64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t umax8(uint8_t x, uint8_t y)
{
    return x < y ? y : x;
}
static inline uint16_t umax16(uint16_t x, uint16_t y)
{
    return x < y ? y : x;
}
static inline uint32_t umax32(uint32_t x, uint32_t y)
{
    return x < y ? y : x;
}
static inline uint64_t umax64(uint64_t x, uint64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t shl8(uint8_t x, uint8_t y)
{
    return x << y;
}
static inline uint16_t shl16(uint16_t x, uint16_t y)
{
    return x << y;
}
static inline uint32_t shl32(uint32_t x, uint32_t y)
{
    return x << y;
}
static inline uint64_t shl64(uint64_t x, uint64_t y)
{
    return x << y;
}
static inline uint8_t lshr8(uint8_t x, uint8_t y)
{
    return x >> y;
}
static inline uint16_t lshr16(uint16_t x, uint16_t y)
{
    return x >> y;
}
static inline uint32_t lshr32(uint32_t x, uint32_t y)
{
    return x >> y;
}
static inline uint64_t lshr64(uint64_t x, uint64_t y)
{
    return x >> y;
}
static inline int8_t ashr8(int8_t x, int8_t y)
{
    return x >> y;
}
static inline int16_t ashr16(int16_t x, int16_t y)
{
    return x >> y;
}
static inline int32_t ashr32(int32_t x, int32_t y)
{
    return x >> y;
}
static inline int64_t ashr64(int64_t x, int64_t y)
{
    return x >> y;
}
static inline uint8_t and8(uint8_t x, uint8_t y)
{
    return x & y;
}
static inline uint16_t and16(uint16_t x, uint16_t y)
{
    return x & y;
}
static inline uint32_t and32(uint32_t x, uint32_t y)
{
    return x & y;
}
static inline uint64_t and64(uint64_t x, uint64_t y)
{
    return x & y;
}
static inline uint8_t or8(uint8_t x, uint8_t y)
{
    return x | y;
}
static inline uint16_t or16(uint16_t x, uint16_t y)
{
    return x | y;
}
static inline uint32_t or32(uint32_t x, uint32_t y)
{
    return x | y;
}
static inline uint64_t or64(uint64_t x, uint64_t y)
{
    return x | y;
}
static inline uint8_t xor8(uint8_t x, uint8_t y)
{
    return x ^ y;
}
static inline uint16_t xor16(uint16_t x, uint16_t y)
{
    return x ^ y;
}
static inline uint32_t xor32(uint32_t x, uint32_t y)
{
    return x ^ y;
}
static inline uint64_t xor64(uint64_t x, uint64_t y)
{
    return x ^ y;
}
static inline char ult8(uint8_t x, uint8_t y)
{
    return x < y;
}
static inline char ult16(uint16_t x, uint16_t y)
{
    return x < y;
}
static inline char ult32(uint32_t x, uint32_t y)
{
    return x < y;
}
static inline char ult64(uint64_t x, uint64_t y)
{
    return x < y;
}
static inline char ule8(uint8_t x, uint8_t y)
{
    return x <= y;
}
static inline char ule16(uint16_t x, uint16_t y)
{
    return x <= y;
}
static inline char ule32(uint32_t x, uint32_t y)
{
    return x <= y;
}
static inline char ule64(uint64_t x, uint64_t y)
{
    return x <= y;
}
static inline char slt8(int8_t x, int8_t y)
{
    return x < y;
}
static inline char slt16(int16_t x, int16_t y)
{
    return x < y;
}
static inline char slt32(int32_t x, int32_t y)
{
    return x < y;
}
static inline char slt64(int64_t x, int64_t y)
{
    return x < y;
}
static inline char sle8(int8_t x, int8_t y)
{
    return x <= y;
}
static inline char sle16(int16_t x, int16_t y)
{
    return x <= y;
}
static inline char sle32(int32_t x, int32_t y)
{
    return x <= y;
}
static inline char sle64(int64_t x, int64_t y)
{
    return x <= y;
}
static inline int8_t pow8(int8_t x, int8_t y)
{
    int8_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int16_t pow16(int16_t x, int16_t y)
{
    int16_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int32_t pow32(int32_t x, int32_t y)
{
    int32_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int64_t pow64(int64_t x, int64_t y)
{
    int64_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int8_t sext_i8_i8(int8_t x)
{
    return x;
}
static inline int16_t sext_i8_i16(int8_t x)
{
    return x;
}
static inline int32_t sext_i8_i32(int8_t x)
{
    return x;
}
static inline int64_t sext_i8_i64(int8_t x)
{
    return x;
}
static inline int8_t sext_i16_i8(int16_t x)
{
    return x;
}
static inline int16_t sext_i16_i16(int16_t x)
{
    return x;
}
static inline int32_t sext_i16_i32(int16_t x)
{
    return x;
}
static inline int64_t sext_i16_i64(int16_t x)
{
    return x;
}
static inline int8_t sext_i32_i8(int32_t x)
{
    return x;
}
static inline int16_t sext_i32_i16(int32_t x)
{
    return x;
}
static inline int32_t sext_i32_i32(int32_t x)
{
    return x;
}
static inline int64_t sext_i32_i64(int32_t x)
{
    return x;
}
static inline int8_t sext_i64_i8(int64_t x)
{
    return x;
}
static inline int16_t sext_i64_i16(int64_t x)
{
    return x;
}
static inline int32_t sext_i64_i32(int64_t x)
{
    return x;
}
static inline int64_t sext_i64_i64(int64_t x)
{
    return x;
}
static inline uint8_t zext_i8_i8(uint8_t x)
{
    return x;
}
static inline uint16_t zext_i8_i16(uint8_t x)
{
    return x;
}
static inline uint32_t zext_i8_i32(uint8_t x)
{
    return x;
}
static inline uint64_t zext_i8_i64(uint8_t x)
{
    return x;
}
static inline uint8_t zext_i16_i8(uint16_t x)
{
    return x;
}
static inline uint16_t zext_i16_i16(uint16_t x)
{
    return x;
}
static inline uint32_t zext_i16_i32(uint16_t x)
{
    return x;
}
static inline uint64_t zext_i16_i64(uint16_t x)
{
    return x;
}
static inline uint8_t zext_i32_i8(uint32_t x)
{
    return x;
}
static inline uint16_t zext_i32_i16(uint32_t x)
{
    return x;
}
static inline uint32_t zext_i32_i32(uint32_t x)
{
    return x;
}
static inline uint64_t zext_i32_i64(uint32_t x)
{
    return x;
}
static inline uint8_t zext_i64_i8(uint64_t x)
{
    return x;
}
static inline uint16_t zext_i64_i16(uint64_t x)
{
    return x;
}
static inline uint32_t zext_i64_i32(uint64_t x)
{
    return x;
}
static inline uint64_t zext_i64_i64(uint64_t x)
{
    return x;
}
static inline float fdiv32(float x, float y)
{
    return x / y;
}
static inline float fadd32(float x, float y)
{
    return x + y;
}
static inline float fsub32(float x, float y)
{
    return x - y;
}
static inline float fmul32(float x, float y)
{
    return x * y;
}
static inline float fmin32(float x, float y)
{
    return x < y ? x : y;
}
static inline float fmax32(float x, float y)
{
    return x < y ? y : x;
}
static inline float fpow32(float x, float y)
{
    return pow(x, y);
}
static inline char cmplt32(float x, float y)
{
    return x < y;
}
static inline char cmple32(float x, float y)
{
    return x <= y;
}
static inline float sitofp_i8_f32(int8_t x)
{
    return x;
}
static inline float sitofp_i16_f32(int16_t x)
{
    return x;
}
static inline float sitofp_i32_f32(int32_t x)
{
    return x;
}
static inline float sitofp_i64_f32(int64_t x)
{
    return x;
}
static inline float uitofp_i8_f32(uint8_t x)
{
    return x;
}
static inline float uitofp_i16_f32(uint16_t x)
{
    return x;
}
static inline float uitofp_i32_f32(uint32_t x)
{
    return x;
}
static inline float uitofp_i64_f32(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f32_i8(float x)
{
    return x;
}
static inline int16_t fptosi_f32_i16(float x)
{
    return x;
}
static inline int32_t fptosi_f32_i32(float x)
{
    return x;
}
static inline int64_t fptosi_f32_i64(float x)
{
    return x;
}
static inline uint8_t fptoui_f32_i8(float x)
{
    return x;
}
static inline uint16_t fptoui_f32_i16(float x)
{
    return x;
}
static inline uint32_t fptoui_f32_i32(float x)
{
    return x;
}
static inline uint64_t fptoui_f32_i64(float x)
{
    return x;
}
static inline double fdiv64(double x, double y)
{
    return x / y;
}
static inline double fadd64(double x, double y)
{
    return x + y;
}
static inline double fsub64(double x, double y)
{
    return x - y;
}
static inline double fmul64(double x, double y)
{
    return x * y;
}
static inline double fmin64(double x, double y)
{
    return x < y ? x : y;
}
static inline double fmax64(double x, double y)
{
    return x < y ? y : x;
}
static inline double fpow64(double x, double y)
{
    return pow(x, y);
}
static inline char cmplt64(double x, double y)
{
    return x < y;
}
static inline char cmple64(double x, double y)
{
    return x <= y;
}
static inline double sitofp_i8_f64(int8_t x)
{
    return x;
}
static inline double sitofp_i16_f64(int16_t x)
{
    return x;
}
static inline double sitofp_i32_f64(int32_t x)
{
    return x;
}
static inline double sitofp_i64_f64(int64_t x)
{
    return x;
}
static inline double uitofp_i8_f64(uint8_t x)
{
    return x;
}
static inline double uitofp_i16_f64(uint16_t x)
{
    return x;
}
static inline double uitofp_i32_f64(uint32_t x)
{
    return x;
}
static inline double uitofp_i64_f64(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f64_i8(double x)
{
    return x;
}
static inline int16_t fptosi_f64_i16(double x)
{
    return x;
}
static inline int32_t fptosi_f64_i32(double x)
{
    return x;
}
static inline int64_t fptosi_f64_i64(double x)
{
    return x;
}
static inline uint8_t fptoui_f64_i8(double x)
{
    return x;
}
static inline uint16_t fptoui_f64_i16(double x)
{
    return x;
}
static inline uint32_t fptoui_f64_i32(double x)
{
    return x;
}
static inline uint64_t fptoui_f64_i64(double x)
{
    return x;
}
static inline float fpconv_f32_f32(float x)
{
    return x;
}
static inline double fpconv_f32_f64(float x)
{
    return x;
}
static inline float fpconv_f64_f32(double x)
{
    return x;
}
static inline double fpconv_f64_f64(double x)
{
    return x;
}
static inline float futrts_log32(float x)
{
    return log(x);
}
static inline float futrts_sqrt32(float x)
{
    return sqrt(x);
}
static inline float futrts_exp32(float x)
{
    return exp(x);
}
static inline float futrts_cos32(float x)
{
    return cos(x);
}
static inline float futrts_sin32(float x)
{
    return sin(x);
}
static inline float futrts_acos32(float x)
{
    return acos(x);
}
static inline float futrts_asin32(float x)
{
    return asin(x);
}
static inline float futrts_atan32(float x)
{
    return atan(x);
}
static inline float futrts_atan2_32(float x, float y)
{
    return atan2(x, y);
}
static inline char futrts_isnan32(float x)
{
    return isnan(x);
}
static inline char futrts_isinf32(float x)
{
    return isinf(x);
}
static inline int32_t futrts_to_bits32(float x)
{
    union {
        float f;
        int32_t t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline float futrts_from_bits32(int32_t x)
{
    union {
        int32_t f;
        float t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline double futrts_log64(double x)
{
    return log(x);
}
static inline double futrts_sqrt64(double x)
{
    return sqrt(x);
}
static inline double futrts_exp64(double x)
{
    return exp(x);
}
static inline double futrts_cos64(double x)
{
    return cos(x);
}
static inline double futrts_sin64(double x)
{
    return sin(x);
}
static inline double futrts_acos64(double x)
{
    return acos(x);
}
static inline double futrts_asin64(double x)
{
    return asin(x);
}
static inline double futrts_atan64(double x)
{
    return atan(x);
}
static inline double futrts_atan2_64(double x, double y)
{
    return atan2(x, y);
}
static inline char futrts_isnan64(double x)
{
    return isnan(x);
}
static inline char futrts_isinf64(double x)
{
    return isinf(x);
}
static inline int64_t futrts_to_bits64(double x)
{
    union {
        double f;
        int64_t t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline double futrts_from_bits64(int64_t x)
{
    union {
        int64_t f;
        double t;
    } p;
    
    p.f = x;
    return p.t;
}
static
struct futrts_int32_t_mem_int32_t_int32_t futrts_main(struct futhark_context *ctx)
{
    int32_t out_memsizze_5112;
    struct memblock out_mem_5111;
    
    out_mem_5111.references = NULL;
    
    int32_t out_arrsizze_5113;
    int32_t out_arrsizze_5114;
    struct memblock mem_5083;
    
    mem_5083.references = NULL;
    memblock_alloc(ctx, &mem_5083, 8400);
    for (int32_t i_5115 = 0; i_5115 < 30; i_5115++) {
        for (int32_t i_5116 = 0; i_5116 < 70; i_5116++) {
            *(int32_t *) &mem_5083.mem[(i_5115 * 70 + i_5116) * 4] = 0;
        }
    }
    
    struct memblock mem_5086;
    
    mem_5086.references = NULL;
    memblock_alloc(ctx, &mem_5086, 8);
    
    struct memblock static_array_5117 = ctx->static_array_5117;
    
    memmove(mem_5086.mem + 0, static_array_5117.mem + 0, 2 * sizeof(int32_t));
    
    struct memblock mem_5089;
    
    mem_5089.references = NULL;
    memblock_alloc(ctx, &mem_5089, 8);
    
    struct memblock static_array_5118 = ctx->static_array_5118;
    
    memmove(mem_5089.mem + 0, static_array_5118.mem + 0, 2 * sizeof(int32_t));
    
    struct memblock mem_5092;
    
    mem_5092.references = NULL;
    memblock_alloc(ctx, &mem_5092, 8);
    
    struct memblock static_array_5119 = ctx->static_array_5119;
    
    memmove(mem_5092.mem + 0, static_array_5119.mem + 0, 2 * sizeof(int32_t));
    
    struct memblock mem_5095;
    
    mem_5095.references = NULL;
    memblock_alloc(ctx, &mem_5095, 8);
    
    struct memblock static_array_5120 = ctx->static_array_5120;
    
    memmove(mem_5095.mem + 0, static_array_5120.mem + 0, 2 * sizeof(int32_t));
    
    struct memblock mem_5098;
    
    mem_5098.references = NULL;
    memblock_alloc(ctx, &mem_5098, 8);
    
    struct memblock mem_5101;
    
    mem_5101.references = NULL;
    memblock_alloc(ctx, &mem_5101, 8);
    
    struct memblock mem_5104;
    
    mem_5104.references = NULL;
    memblock_alloc(ctx, &mem_5104, 8);
    
    struct memblock mem_5107;
    
    mem_5107.references = NULL;
    memblock_alloc(ctx, &mem_5107, 8);
    for (int32_t i_5059 = 0; i_5059 < 2; i_5059++) {
        int32_t not_curried_4831 = *(int32_t *) &mem_5086.mem[i_5059 * 4];
        int32_t not_curried_4832 = *(int32_t *) &mem_5089.mem[i_5059 * 4];
        int32_t not_curried_4833 = *(int32_t *) &mem_5092.mem[i_5059 * 4];
        int32_t not_curried_4834 = *(int32_t *) &mem_5095.mem[i_5059 * 4];
        double res_4835 = sitofp_i32_f64(not_curried_4831);
        double res_4836 = sitofp_i32_f64(not_curried_4832);
        double y_4837 = 7.963267107332633e-4 * res_4835;
        double y_4838 = 0.9999996829318346 * res_4836;
        double arg_4839 = y_4837 - y_4838;
        double res_4840 = arg_4839 + 0.5;
        int64_t res_4841 = fptosi_f64_i64(res_4840);
        double res_4842 = sitofp_i64_f64(res_4841);
        char res_4843 = 0.0 <= res_4840;
        double res_4844;
        
        if (res_4843) {
            char res_4845 = res_4842 < res_4840;
            double res_4846;
            
            if (res_4845) {
                res_4846 = res_4842;
            } else {
                res_4846 = res_4840;
            }
            res_4844 = res_4846;
        } else {
            char res_4847 = res_4840 < res_4842;
            double res_4848;
            
            if (res_4847) {
                int64_t res_4849 = res_4841 - 1;
                double res_4850 = sitofp_i64_f64(res_4849);
                
                res_4848 = res_4850;
            } else {
                res_4848 = res_4840;
            }
            res_4844 = res_4848;
        }
        
        char res_4851 = res_4844 == res_4840;
        double res_4852;
        
        if (res_4851) {
            int64_t res_4853 = fptosi_f64_i64(arg_4839);
            double res_4854 = sitofp_i64_f64(res_4853);
            char res_4855 = 0.0 <= arg_4839;
            double res_4856;
            
            if (res_4855) {
                char res_4857 = res_4854 < arg_4839;
                double res_4858;
                
                if (res_4857) {
                    res_4858 = res_4854;
                } else {
                    res_4858 = arg_4839;
                }
                res_4856 = res_4858;
            } else {
                char res_4859 = arg_4839 < res_4854;
                double res_4860;
                
                if (res_4859) {
                    int64_t res_4861 = res_4853 - 1;
                    double res_4862 = sitofp_i64_f64(res_4861);
                    
                    res_4860 = res_4862;
                } else {
                    res_4860 = arg_4839;
                }
                res_4856 = res_4860;
            }
            
            int64_t res_4863 = fptosi_f64_i64(res_4856);
            int64_t arg_4864 = smod64(res_4863, 2);
            char res_4865 = arg_4864 == 0;
            double res_4866;
            
            if (res_4865) {
                res_4866 = res_4856;
            } else {
                res_4866 = res_4844;
            }
            res_4852 = res_4866;
        } else {
            res_4852 = res_4844;
        }
        
        double y_4867 = 0.9999996829318346 * res_4835;
        double y_4868 = 7.963267107332633e-4 * res_4836;
        double arg_4869 = y_4867 + y_4868;
        double res_4870 = arg_4869 + 0.5;
        int64_t res_4871 = fptosi_f64_i64(res_4870);
        double res_4872 = sitofp_i64_f64(res_4871);
        char res_4873 = 0.0 <= res_4870;
        double res_4874;
        
        if (res_4873) {
            char res_4875 = res_4872 < res_4870;
            double res_4876;
            
            if (res_4875) {
                res_4876 = res_4872;
            } else {
                res_4876 = res_4870;
            }
            res_4874 = res_4876;
        } else {
            char res_4877 = res_4870 < res_4872;
            double res_4878;
            
            if (res_4877) {
                int64_t res_4879 = res_4871 - 1;
                double res_4880 = sitofp_i64_f64(res_4879);
                
                res_4878 = res_4880;
            } else {
                res_4878 = res_4870;
            }
            res_4874 = res_4878;
        }
        
        char res_4881 = res_4874 == res_4870;
        double res_4882;
        
        if (res_4881) {
            int64_t res_4883 = fptosi_f64_i64(arg_4869);
            double res_4884 = sitofp_i64_f64(res_4883);
            char res_4885 = 0.0 <= arg_4869;
            double res_4886;
            
            if (res_4885) {
                char res_4887 = res_4884 < arg_4869;
                double res_4888;
                
                if (res_4887) {
                    res_4888 = res_4884;
                } else {
                    res_4888 = arg_4869;
                }
                res_4886 = res_4888;
            } else {
                char res_4889 = arg_4869 < res_4884;
                double res_4890;
                
                if (res_4889) {
                    int64_t res_4891 = res_4883 - 1;
                    double res_4892 = sitofp_i64_f64(res_4891);
                    
                    res_4890 = res_4892;
                } else {
                    res_4890 = arg_4869;
                }
                res_4886 = res_4890;
            }
            
            int64_t res_4893 = fptosi_f64_i64(res_4886);
            int64_t arg_4894 = smod64(res_4893, 2);
            char res_4895 = arg_4894 == 0;
            double res_4896;
            
            if (res_4895) {
                res_4896 = res_4886;
            } else {
                res_4896 = res_4874;
            }
            res_4882 = res_4896;
        } else {
            res_4882 = res_4874;
        }
        
        int32_t res_4897 = fptosi_f64_i32(res_4852);
        int32_t res_4898 = fptosi_f64_i32(res_4882);
        double res_4899 = sitofp_i32_f64(not_curried_4833);
        double res_4900 = sitofp_i32_f64(not_curried_4834);
        double y_4901 = 7.963267107332633e-4 * res_4899;
        double y_4902 = 0.9999996829318346 * res_4900;
        double arg_4903 = y_4901 - y_4902;
        double res_4904 = arg_4903 + 0.5;
        int64_t res_4905 = fptosi_f64_i64(res_4904);
        double res_4906 = sitofp_i64_f64(res_4905);
        char res_4907 = 0.0 <= res_4904;
        double res_4908;
        
        if (res_4907) {
            char res_4909 = res_4906 < res_4904;
            double res_4910;
            
            if (res_4909) {
                res_4910 = res_4906;
            } else {
                res_4910 = res_4904;
            }
            res_4908 = res_4910;
        } else {
            char res_4911 = res_4904 < res_4906;
            double res_4912;
            
            if (res_4911) {
                int64_t res_4913 = res_4905 - 1;
                double res_4914 = sitofp_i64_f64(res_4913);
                
                res_4912 = res_4914;
            } else {
                res_4912 = res_4904;
            }
            res_4908 = res_4912;
        }
        
        char res_4915 = res_4908 == res_4904;
        double res_4916;
        
        if (res_4915) {
            int64_t res_4917 = fptosi_f64_i64(arg_4903);
            double res_4918 = sitofp_i64_f64(res_4917);
            char res_4919 = 0.0 <= arg_4903;
            double res_4920;
            
            if (res_4919) {
                char res_4921 = res_4918 < arg_4903;
                double res_4922;
                
                if (res_4921) {
                    res_4922 = res_4918;
                } else {
                    res_4922 = arg_4903;
                }
                res_4920 = res_4922;
            } else {
                char res_4923 = arg_4903 < res_4918;
                double res_4924;
                
                if (res_4923) {
                    int64_t res_4925 = res_4917 - 1;
                    double res_4926 = sitofp_i64_f64(res_4925);
                    
                    res_4924 = res_4926;
                } else {
                    res_4924 = arg_4903;
                }
                res_4920 = res_4924;
            }
            
            int64_t res_4927 = fptosi_f64_i64(res_4920);
            int64_t arg_4928 = smod64(res_4927, 2);
            char res_4929 = arg_4928 == 0;
            double res_4930;
            
            if (res_4929) {
                res_4930 = res_4920;
            } else {
                res_4930 = res_4908;
            }
            res_4916 = res_4930;
        } else {
            res_4916 = res_4908;
        }
        
        double y_4931 = 0.9999996829318346 * res_4899;
        double y_4932 = 7.963267107332633e-4 * res_4900;
        double arg_4933 = y_4931 + y_4932;
        double res_4934 = arg_4933 + 0.5;
        int64_t res_4935 = fptosi_f64_i64(res_4934);
        double res_4936 = sitofp_i64_f64(res_4935);
        char res_4937 = 0.0 <= res_4934;
        double res_4938;
        
        if (res_4937) {
            char res_4939 = res_4936 < res_4934;
            double res_4940;
            
            if (res_4939) {
                res_4940 = res_4936;
            } else {
                res_4940 = res_4934;
            }
            res_4938 = res_4940;
        } else {
            char res_4941 = res_4934 < res_4936;
            double res_4942;
            
            if (res_4941) {
                int64_t res_4943 = res_4935 - 1;
                double res_4944 = sitofp_i64_f64(res_4943);
                
                res_4942 = res_4944;
            } else {
                res_4942 = res_4934;
            }
            res_4938 = res_4942;
        }
        
        char res_4945 = res_4938 == res_4934;
        double res_4946;
        
        if (res_4945) {
            int64_t res_4947 = fptosi_f64_i64(arg_4933);
            double res_4948 = sitofp_i64_f64(res_4947);
            char res_4949 = 0.0 <= arg_4933;
            double res_4950;
            
            if (res_4949) {
                char res_4951 = res_4948 < arg_4933;
                double res_4952;
                
                if (res_4951) {
                    res_4952 = res_4948;
                } else {
                    res_4952 = arg_4933;
                }
                res_4950 = res_4952;
            } else {
                char res_4953 = arg_4933 < res_4948;
                double res_4954;
                
                if (res_4953) {
                    int64_t res_4955 = res_4947 - 1;
                    double res_4956 = sitofp_i64_f64(res_4955);
                    
                    res_4954 = res_4956;
                } else {
                    res_4954 = arg_4933;
                }
                res_4950 = res_4954;
            }
            
            int64_t res_4957 = fptosi_f64_i64(res_4950);
            int64_t arg_4958 = smod64(res_4957, 2);
            char res_4959 = arg_4958 == 0;
            double res_4960;
            
            if (res_4959) {
                res_4960 = res_4950;
            } else {
                res_4960 = res_4938;
            }
            res_4946 = res_4960;
        } else {
            res_4946 = res_4938;
        }
        
        int32_t res_4961 = fptosi_f64_i32(res_4916);
        int32_t res_4962 = fptosi_f64_i32(res_4946);
        
        *(int32_t *) &mem_5098.mem[i_5059 * 4] = res_4897;
        *(int32_t *) &mem_5101.mem[i_5059 * 4] = res_4898;
        *(int32_t *) &mem_5104.mem[i_5059 * 4] = res_4961;
        *(int32_t *) &mem_5107.mem[i_5059 * 4] = res_4962;
    }
    for (int32_t i_4965 = 0; i_4965 < 2; i_4965++) {
        int32_t arg_4966 = *(int32_t *) &mem_5098.mem[i_4965 * 4];
        int32_t arg_4967 = *(int32_t *) &mem_5101.mem[i_4965 * 4];
        int32_t arg_4968 = *(int32_t *) &mem_5104.mem[i_4965 * 4];
        int32_t arg_4969 = *(int32_t *) &mem_5107.mem[i_4965 * 4];
        int32_t arg_4970 = arg_4968 - arg_4966;
        int32_t res_4971 = abs(arg_4970);
        int32_t arg_4972 = arg_4969 - arg_4967;
        int32_t res_4973 = abs(arg_4972);
        int32_t res_4974 = smax32(res_4971, res_4973);
        int32_t res_4975 = 1 + res_4974;
        char res_4976 = slt32(res_4973, res_4971);
        int32_t res_4977;
        float res_4978;
        
        if (res_4976) {
            char cond_4979 = slt32(arg_4966, arg_4968);
            int32_t res_4980;
            
            if (cond_4979) {
                res_4980 = 1;
            } else {
                char cond_4981 = slt32(arg_4968, arg_4966);
                int32_t res_4982;
                
                if (cond_4981) {
                    res_4982 = -1;
                } else {
                    res_4982 = 0;
                }
                res_4980 = res_4982;
            }
            
            char cond_4983 = arg_4968 == arg_4966;
            float res_4984;
            
            if (cond_4983) {
                char cond_4985 = slt32(arg_4967, arg_4969);
                float res_4986;
                
                if (cond_4985) {
                    res_4986 = 1.0F;
                } else {
                    res_4986 = -1.0F;
                }
                res_4984 = res_4986;
            } else {
                float res_4988 = sitofp_i32_f32(arg_4972);
                float res_4990 = sitofp_i32_f32(arg_4970);
                float res_4991 = (float) fabs(res_4990);
                float res_4992 = res_4988 / res_4991;
                
                res_4984 = res_4992;
            }
            res_4977 = res_4980;
            res_4978 = res_4984;
        } else {
            char cond_4993 = slt32(arg_4967, arg_4969);
            int32_t res_4994;
            
            if (cond_4993) {
                res_4994 = 1;
            } else {
                char cond_4995 = slt32(arg_4969, arg_4967);
                int32_t res_4996;
                
                if (cond_4995) {
                    res_4996 = -1;
                } else {
                    res_4996 = 0;
                }
                res_4994 = res_4996;
            }
            
            char cond_4997 = arg_4969 == arg_4967;
            float res_4998;
            
            if (cond_4997) {
                char cond_4999 = slt32(arg_4966, arg_4968);
                float res_5000;
                
                if (cond_4999) {
                    res_5000 = 1.0F;
                } else {
                    res_5000 = -1.0F;
                }
                res_4998 = res_5000;
            } else {
                float res_5002 = sitofp_i32_f32(arg_4970);
                float res_5004 = sitofp_i32_f32(arg_4972);
                float res_5005 = (float) fabs(res_5004);
                float res_5006 = res_5002 / res_5005;
                
                res_4998 = res_5006;
            }
            res_4977 = res_4994;
            res_4978 = res_4998;
        }
        for (int32_t write_iter_5072 = 0; write_iter_5072 < res_4975;
             write_iter_5072++) {
            int32_t res_5039;
            int32_t res_5040;
            
            if (res_4976) {
                int32_t y_5041 = write_iter_5072 * res_4977;
                int32_t res_5042 = arg_4966 + y_5041;
                float res_5043 = sitofp_i32_f32(write_iter_5072);
                float arg_5044 = res_4978 * res_5043;
                int32_t res_5045 = fptosi_f32_i32(arg_5044);
                int32_t res_5046 = arg_4967 + res_5045;
                
                res_5039 = res_5042;
                res_5040 = res_5046;
            } else {
                float res_5047 = sitofp_i32_f32(write_iter_5072);
                float arg_5048 = res_4978 * res_5047;
                int32_t res_5049 = fptosi_f32_i32(arg_5048);
                int32_t res_5050 = arg_4966 + res_5049;
                int32_t y_5051 = write_iter_5072 * res_4977;
                int32_t res_5052 = arg_4967 + y_5051;
                
                res_5039 = res_5050;
                res_5040 = res_5052;
            }
            
            int32_t x_5053 = 70 * res_5040;
            int32_t res_5054 = x_5053 + res_5039;
            char less_than_zzero_5075 = slt32(res_5054, 0);
            char greater_than_sizze_5076 = sle32(2100, res_5054);
            char outside_bounds_5077 = less_than_zzero_5075 ||
                 greater_than_sizze_5076;
            
            if (!outside_bounds_5077) {
                *(int32_t *) &mem_5083.mem[res_5054 * 4] = 1;
            }
        }
    }
    out_arrsizze_5113 = 30;
    out_arrsizze_5114 = 70;
    out_memsizze_5112 = 8400;
    memblock_set(ctx, &out_mem_5111, &mem_5083);
    
    struct futrts_int32_t_mem_int32_t_int32_t retval_5127;
    
    retval_5127.v0 = out_memsizze_5112;
    retval_5127.v1.references = NULL;
    memblock_set(ctx, &retval_5127.v1, &out_mem_5111);
    retval_5127.v2 = out_arrsizze_5113;
    retval_5127.v3 = out_arrsizze_5114;
    memblock_unref(ctx, &out_mem_5111);
    memblock_unref(ctx, &mem_5083);
    memblock_unref(ctx, &mem_5086);
    memblock_unref(ctx, &mem_5089);
    memblock_unref(ctx, &mem_5092);
    memblock_unref(ctx, &mem_5095);
    memblock_unref(ctx, &mem_5098);
    memblock_unref(ctx, &mem_5101);
    memblock_unref(ctx, &mem_5104);
    memblock_unref(ctx, &mem_5107);
    return retval_5127;
}
struct futhark_i32_2d {
    struct memblock mem;
    int64_t shape[2];
} ;
struct futhark_i32_2d *futhark_new_i32_2d(struct futhark_context *ctx,
                                          int32_t *data, int dim0, int dim1)
{
    struct futhark_i32_2d *arr = malloc(sizeof(struct futhark_i32_2d));
    
    if (arr == NULL)
        return NULL;
    arr->mem.references = NULL;
    memblock_alloc(ctx, &arr->mem, dim0 * dim1 * sizeof(int32_t));
    memmove(arr->mem.mem + 0, data + 0, dim0 * dim1 * sizeof(int32_t));
    arr->shape[0] = dim0;
    arr->shape[1] = dim1;
    return arr;
}
int futhark_free_i32_2d(struct futhark_context *ctx, struct futhark_i32_2d *arr)
{
    memblock_unref(ctx, &arr->mem);
    free(arr);
    return 0;
}
int futhark_values_i32_2d(struct futhark_context *ctx,
                          struct futhark_i32_2d *arr, int32_t *data)
{
    memmove(data + 0, arr->mem.mem + 0, arr->shape[0] * arr->shape[1] *
            sizeof(int32_t));
    return 0;
}
int64_t *futhark_shape_i32_2d(struct futhark_context *ctx,
                              struct futhark_i32_2d *arr)
{
    return arr->shape;
}
int futhark_main(struct futhark_context *ctx, struct futhark_i32_2d **out0)
{
    int32_t out_memsizze_5112;
    struct memblock out_mem_5111;
    
    out_mem_5111.references = NULL;
    
    int32_t out_arrsizze_5113;
    int32_t out_arrsizze_5114;
    struct futrts_int32_t_mem_int32_t_int32_t ret_5132;
    
    ret_5132 = futrts_main(ctx);
    out_memsizze_5112 = ret_5132.v0;
    out_mem_5111 = ret_5132.v1;
    out_arrsizze_5113 = ret_5132.v2;
    out_arrsizze_5114 = ret_5132.v3;
    assert((*out0 = malloc(sizeof(struct futhark_i32_2d))) != NULL);
    (*out0)->mem = out_mem_5111;
    (*out0)->shape[0] = out_arrsizze_5113;
    (*out0)->shape[1] = out_arrsizze_5114;
    return 0;
}
