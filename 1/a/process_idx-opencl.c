#include <stdint.h>

/*
 * Initialisation
*/

struct futhark_context_config ;
struct futhark_context_config *futhark_context_config_new();
void futhark_context_config_free(struct futhark_context_config *cfg);
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_device(struct futhark_context_config *cfg, const
                                       char *s);
void futhark_context_config_set_platform(struct futhark_context_config *cfg,
                                         const char *s);
void futhark_context_config_dump_program_to(struct futhark_context_config *cfg,
                                            const char *path);
void
futhark_context_config_load_program_from(struct futhark_context_config *cfg,
                                         const char *path);
void futhark_context_config_set_group_size(struct futhark_context_config *cfg,
                                           int size);
void futhark_context_config_set_num_groups(struct futhark_context_config *cfg,
                                           int num);
struct futhark_context ;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);
void futhark_context_free(struct futhark_context *ctx);
int futhark_context_sync(struct futhark_context *ctx);

/*
 * Arrays
*/

struct futhark_i32_1d ;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                          int32_t *data, int dim0);
int futhark_free_i32_1d(struct futhark_context *ctx,
                        struct futhark_i32_1d *arr);
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data);
int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                              struct futhark_i32_1d *arr);

/*
 * Opaque values
*/


/*
 * Entry points
*/

int futhark_main(struct futhark_context *ctx, int32_t *out0, int32_t *out1,
                 struct futhark_i32_1d *in0, struct futhark_i32_1d *in1);

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
                                           {"platform", required_argument, NULL,
                                            6}, {"device", required_argument,
                                                 NULL, 7}, {"group-size",
                                                            required_argument,
                                                            NULL, 8},
                                           {"num-groups", required_argument,
                                            NULL, 9}, {"dump-opencl",
                                                       required_argument, NULL,
                                                       10}, {"load-opencl",
                                                             required_argument,
                                                             NULL, 11}, {0, 0,
                                                                         0, 0}};
    
    while ((ch = getopt_long(argc, argv, ":t:r:De:bp:d:", long_options,
                             NULL)) != -1) {
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
        if (ch == 6 || ch == 'p')
            futhark_context_config_set_platform(cfg, optarg);
        if (ch == 7 || ch == 'd')
            futhark_context_config_set_device(cfg, optarg);
        if (ch == 8)
            futhark_context_config_set_group_size(cfg, atoi(optarg));
        if (ch == 9)
            futhark_context_config_set_num_groups(cfg, atoi(optarg));
        if (ch == 10)
            futhark_context_config_dump_program_to(cfg, optarg);
        if (ch == 11)
            futhark_context_config_load_program_from(cfg, optarg);
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
    struct futhark_i32_1d *read_value_3522;
    int64_t read_shape_3523[1];
    int32_t *read_arr_3524 = NULL;
    
    errno = 0;
    if (read_array(&i32, (void **) &read_arr_3524, read_shape_3523, 1) != 0)
        panic(1, "Failed reading input of type %s%s (errno: %s).\n", "[]",
              i32.type_name, strerror(errno));
    
    struct futhark_i32_1d *read_value_3525;
    int64_t read_shape_3526[1];
    int32_t *read_arr_3527 = NULL;
    
    errno = 0;
    if (read_array(&i32, (void **) &read_arr_3527, read_shape_3526, 1) != 0)
        panic(1, "Failed reading input of type %s%s (errno: %s).\n", "[]",
              i32.type_name, strerror(errno));
    
    int32_t result_3528;
    int32_t result_3529;
    
    if (perform_warmup) {
        time_runs = 0;
        assert((read_value_3522 = futhark_new_i32_1d(ctx, read_arr_3524,
                                                     read_shape_3523[0])) != 0);
        assert((read_value_3525 = futhark_new_i32_1d(ctx, read_arr_3527,
                                                     read_shape_3526[0])) != 0);
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_3528, &result_3529, read_value_3522,
                            read_value_3525) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        assert(futhark_free_i32_1d(ctx, read_value_3522) == 0);
        assert(futhark_free_i32_1d(ctx, read_value_3525) == 0);
        ;
        ;
    }
    time_runs = 1;
    /* Proper run. */
    for (int run = 0; run < num_runs; run++) {
        assert((read_value_3522 = futhark_new_i32_1d(ctx, read_arr_3524,
                                                     read_shape_3523[0])) != 0);
        assert((read_value_3525 = futhark_new_i32_1d(ctx, read_arr_3527,
                                                     read_shape_3526[0])) != 0);
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_3528, &result_3529, read_value_3522,
                            read_value_3525) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        assert(futhark_free_i32_1d(ctx, read_value_3522) == 0);
        assert(futhark_free_i32_1d(ctx, read_value_3525) == 0);
        if (run < num_runs - 1) {
            ;
            ;
        }
    }
    free(read_arr_3524);
    free(read_arr_3527);
    write_scalar(stdout, binary_output, &i32, &result_3528);
    printf("\n");
    write_scalar(stdout, binary_output, &i32, &result_3529);
    printf("\n");
    ;
    ;
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
/* The simple OpenCL runtime framework used by Futhark. */

#ifdef __APPLE__
  #include <OpenCL/cl.h>
#else
  #include <CL/cl.h>
#endif

#define OPENCL_SUCCEED(e) opencl_succeed(e, #e, __FILE__, __LINE__)

struct opencl_config {
  int debugging;
  int preferred_device_num;
  const char *preferred_platform;
  const char *preferred_device;

  const char* dump_program_to;
  const char* load_program_from;

  size_t group_size;
  size_t num_groups;
  size_t tile_size;
  size_t transpose_block_dim;
};

void opencl_config_init(struct opencl_config *cfg) {
  cfg->debugging = 0;
  cfg->preferred_device_num = 0;
  cfg->preferred_platform = "";
  cfg->preferred_device = "";
  cfg->dump_program_to = NULL;
  cfg->load_program_from = NULL;

  cfg->group_size = 0;
  cfg->num_groups = 0;
  cfg->tile_size = 32;
  cfg->transpose_block_dim = 16;
}

struct opencl_context {
  cl_platform_id platform;
  cl_device_id device;
  cl_context ctx;
  cl_command_queue queue;

  struct opencl_config cfg;

  size_t lockstep_width;
};

struct opencl_device_option {
  cl_platform_id platform;
  cl_device_id device;
  cl_device_type device_type;
  char *platform_name;
  char *device_name;
};

/* This function must be defined by the user.  It is invoked by
   setup_opencl() after the platform and device has been found, but
   before the program is loaded.  Its intended use is to tune
   constants based on the selected platform and device. */
static void post_opencl_setup(struct opencl_context*, struct opencl_device_option*);

static char *strclone(const char *str) {
  size_t size = strlen(str) + 1;
  char *copy = malloc(size);
  if (copy == NULL) {
    return NULL;
  }

  memcpy(copy, str, size);
  return copy;
}

static const char* opencl_error_string(unsigned int err)
{
    switch (err) {
        case CL_SUCCESS:                            return "Success!";
        case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
        case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES:                   return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
        case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
        case CL_MAP_FAILURE:                        return "Map failure";
        case CL_INVALID_VALUE:                      return "Invalid value";
        case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
        case CL_INVALID_PLATFORM:                   return "Invalid platform";
        case CL_INVALID_DEVICE:                     return "Invalid device";
        case CL_INVALID_CONTEXT:                    return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
        case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
        case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
        case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
        case CL_INVALID_SAMPLER:                    return "Invalid sampler";
        case CL_INVALID_BINARY:                     return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
        case CL_INVALID_PROGRAM:                    return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
        case CL_INVALID_KERNEL:                     return "Invalid kernel";
        case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
        case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
        case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
        case CL_INVALID_EVENT:                      return "Invalid event";
        case CL_INVALID_OPERATION:                  return "Invalid operation";
        case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
        case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
        default:                                    return "Unknown";
    }
}

static void opencl_succeed(unsigned int ret,
                           const char *call,
                           const char *file,
                           int line) {
  if (ret != CL_SUCCESS) {
    panic(-1, "%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, opencl_error_string(ret));
  }
}

void set_preferred_platform(struct opencl_config *cfg, const char *s) {
  cfg->preferred_platform = s;
}

void set_preferred_device(struct opencl_config *cfg, const char *s) {
  int x = 0;
  if (*s == '#') {
    s++;
    while (isdigit(*s)) {
      x = x * 10 + (*s++)-'0';
    }
    // Skip trailing spaces.
    while (isspace(*s)) {
      s++;
    }
  }
  cfg->preferred_device = s;
  cfg->preferred_device_num = x;
}

static char* opencl_platform_info(cl_platform_id platform,
                                  cl_platform_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, req_bytes, info, NULL));

  return info;
}

static char* opencl_device_info(cl_device_id device,
                                cl_device_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, req_bytes, info, NULL));

  return info;
}

static void opencl_all_device_options(struct opencl_device_option **devices_out,
                                      size_t *num_devices_out) {
  size_t num_devices = 0, num_devices_added = 0;

  cl_platform_id *all_platforms;
  cl_uint *platform_num_devices;

  cl_uint num_platforms;

  // Find the number of platforms.
  OPENCL_SUCCEED(clGetPlatformIDs(0, NULL, &num_platforms));

  // Make room for them.
  all_platforms = calloc(num_platforms, sizeof(cl_platform_id));
  platform_num_devices = calloc(num_platforms, sizeof(cl_uint));

  // Fetch all the platforms.
  OPENCL_SUCCEED(clGetPlatformIDs(num_platforms, all_platforms, NULL));

  // Count the number of devices for each platform, as well as the
  // total number of devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    if (clGetDeviceIDs(all_platforms[i], CL_DEVICE_TYPE_ALL,
                       0, NULL, &platform_num_devices[i]) == CL_SUCCESS) {
      num_devices += platform_num_devices[i];
    } else {
      platform_num_devices[i] = 0;
    }
  }

  // Make room for all the device options.
  struct opencl_device_option *devices =
    calloc(num_devices, sizeof(struct opencl_device_option));

  // Loop through the platforms, getting information about their devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    cl_platform_id platform = all_platforms[i];
    cl_uint num_platform_devices = platform_num_devices[i];

    if (num_platform_devices == 0) {
      continue;
    }

    char *platform_name = opencl_platform_info(platform, CL_PLATFORM_NAME);
    cl_device_id *platform_devices =
      calloc(num_platform_devices, sizeof(cl_device_id));

    // Fetch all the devices.
    OPENCL_SUCCEED(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL,
                                  num_platform_devices, platform_devices, NULL));

    // Loop through the devices, adding them to the devices array.
    for (cl_uint i = 0; i < num_platform_devices; i++) {
      char *device_name = opencl_device_info(platform_devices[i], CL_DEVICE_NAME);
      devices[num_devices_added].platform = platform;
      devices[num_devices_added].device = platform_devices[i];
      OPENCL_SUCCEED(clGetDeviceInfo(platform_devices[i], CL_DEVICE_TYPE,
                                     sizeof(cl_device_type),
                                     &devices[num_devices_added].device_type,
                                     NULL));
      // We don't want the structs to share memory, so copy the platform name.
      // Each device name is already unique.
      devices[num_devices_added].platform_name = strclone(platform_name);
      devices[num_devices_added].device_name = device_name;
      num_devices_added++;
    }
    free(platform_devices);
    free(platform_name);
  }
  free(all_platforms);
  free(platform_num_devices);

  *devices_out = devices;
  *num_devices_out = num_devices;
}

static struct opencl_device_option get_preferred_device(const struct opencl_config *cfg) {
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  int num_platform_matches = 0;
  int num_device_matches = 0;

  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strstr(device.platform_name, cfg->preferred_platform) != NULL &&
        strstr(device.device_name, cfg->preferred_device) != NULL &&
        num_device_matches++ == cfg->preferred_device_num) {
      // Free all the platform and device names, except the ones we have chosen.
      for (size_t j = 0; j < num_devices; j++) {
        if (j != i) {
          free(devices[j].platform_name);
          free(devices[j].device_name);
        }
      }
      free(devices);
      return device;
    }
  }

  panic(1, "Could not find acceptable OpenCL device.\n");
  exit(1); // Never reached
}

static void describe_device_option(struct opencl_device_option device) {
  fprintf(stderr, "Using platform: %s\n", device.platform_name);
  fprintf(stderr, "Using device: %s\n", device.device_name);
}

static cl_build_status build_opencl_program(cl_program program, cl_device_id device, const char* options) {
  cl_int ret_val = clBuildProgram(program, 1, &device, options, NULL, NULL);

  // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
  if (ret_val != CL_SUCCESS && ret_val != CL_BUILD_PROGRAM_FAILURE) {
    assert(ret_val == 0);
  }

  cl_build_status build_status;
  ret_val = clGetProgramBuildInfo(program,
                                  device,
                                  CL_PROGRAM_BUILD_STATUS,
                                  sizeof(cl_build_status),
                                  &build_status,
                                  NULL);
  assert(ret_val == 0);

  if (build_status != CL_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    ret_val = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size);
    assert(ret_val == 0);

    build_log = malloc(ret_val_size+1);
    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL);
    assert(ret_val == 0);

    // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';

    fprintf(stderr, "Build log:\n%s\n", build_log);

    free(build_log);
  }

  return build_status;
}

// We take as input several strings representing the program, because
// C does not guarantee that the compiler supports particularly large
// literals.  Notably, Visual C has a limit of 2048 characters.  The
// array must be NULL-terminated.
static cl_program setup_opencl(struct opencl_context *ctx,
                               const char *srcs[]) {

  cl_int error;
  cl_platform_id platform;
  cl_device_id device;
  cl_uint platforms, devices;
  size_t max_group_size;

  ctx->lockstep_width = 0;

  struct opencl_device_option device_option = get_preferred_device(&ctx->cfg);

  if (ctx->cfg.debugging) {
    describe_device_option(device_option);
  }

  ctx->device = device = device_option.device;
  ctx->platform = platform = device_option.platform;

  OPENCL_SUCCEED(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                                 sizeof(size_t), &max_group_size, NULL));

  size_t max_tile_size = sqrt(max_group_size);

  if (max_group_size < ctx->cfg.group_size) {
    fprintf(stderr, "Warning: Device limits group size to %zu (setting was %zu)\n",
            max_group_size, ctx->cfg.group_size);
    ctx->cfg.group_size = max_group_size;
  }

  if (max_tile_size < ctx->cfg.tile_size) {
    fprintf(stderr, "Warning: Device limits tile size to %zu (setting was %zu)\n",
            max_tile_size, ctx->cfg.tile_size);
    ctx->cfg.tile_size = max_tile_size;
  }

  cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (cl_context_properties)platform,
    0
  };
  // Note that nVidia's OpenCL requires the platform property
  ctx->ctx = clCreateContext(properties, 1, &device, NULL, NULL, &error);
  assert(error == 0);

  ctx->queue = clCreateCommandQueue(ctx->ctx, device, 0, &error);
  assert(error == 0);

  // Make sure this function is defined.
  post_opencl_setup(ctx, &device_option);

  if (ctx->cfg.debugging) {
    fprintf(stderr, "Lockstep width: %d\n", ctx->lockstep_width);
    fprintf(stderr, "Default group size: %d\n", ctx->cfg.group_size);
    fprintf(stderr, "Default number of groups: %d\n", ctx->cfg.num_groups);
  }

  char *fut_opencl_src = NULL;
  size_t src_size = 0;

  // Maybe we have to read OpenCL source from somewhere else (used for debugging).
  if (ctx->cfg.load_program_from != NULL) {
    FILE *f = fopen(ctx->cfg.load_program_from, "r");
    assert(f != NULL);
    fseek(f, 0, SEEK_END);
    src_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    fut_opencl_src = malloc(src_size);
    fread(fut_opencl_src, 1, src_size, f);
    fclose(f);
  } else {
    // Build the OpenCL program.  First we have to concatenate all the fragments.
    for (const char **src = srcs; *src; src++) {
      src_size += strlen(*src);
    }

    fut_opencl_src = malloc(src_size + 1);

    size_t n, i;
    for (i = 0, n = 0; srcs[i]; i++) {
      strncpy(fut_opencl_src+n, srcs[i], src_size-n);
      n += strlen(srcs[i]);
    }
    fut_opencl_src[src_size] = 0;

  }

  cl_program prog;
  error = 0;
  const char* src_ptr[] = {fut_opencl_src};

  if (ctx->cfg.dump_program_to != NULL) {
    FILE *f = fopen(ctx->cfg.dump_program_to, "w");
    assert(f != NULL);
    fputs(fut_opencl_src, f);
    fclose(f);
  }

  prog = clCreateProgramWithSource(ctx->ctx, 1, src_ptr, &src_size, &error);
  assert(error == 0);
  char compile_opts[1024];
  snprintf(compile_opts, sizeof(compile_opts), "-DFUT_BLOCK_DIM=%d -DLOCKSTEP_WIDTH=%d -DDEFAULT_GROUP_SIZE=%d -DDEFAULT_NUM_GROUPS=%d  -DDEFAULT_TILE_SIZE=%d",
           (int)ctx->cfg.transpose_block_dim,
           (int)ctx->lockstep_width,
           (int)ctx->cfg.group_size,
           (int)ctx->cfg.num_groups,
           (int)ctx->cfg.tile_size);
  OPENCL_SUCCEED(build_opencl_program(prog, device, compile_opts));
  free(fut_opencl_src);

  return prog;
}

const char *opencl_program[] =
           {"__kernel void dummy_kernel(__global unsigned char *dummy, int n)\n{\n    const int thread_gid = get_global_id(0);\n    \n    if (thread_gid >= n)\n        return;\n}\ntypedef char int8_t;\ntypedef short int16_t;\ntypedef int int32_t;\ntypedef long int64_t;\ntypedef uchar uint8_t;\ntypedef ushort uint16_t;\ntypedef uint uint32_t;\ntypedef ulong uint64_t;\n#define ALIGNED_LOCAL_MEMORY(m,size) __local unsigned char m[size] __attribute__ ((align))\nstatic inline int8_t add8(int8_t x, int8_t y)\n{\n    return x + y;\n}\nstatic inline int16_t add16(int16_t x, int16_t y)\n{\n    return x + y;\n}\nstatic inline int32_t add32(int32_t x, int32_t y)\n{\n    return x + y;\n}\nstatic inline int64_t add64(int64_t x, int64_t y)\n{\n    return x + y;\n}\nstatic inline int8_t sub8(int8_t x, int8_t y)\n{\n    return x - y;\n}\nstatic inline int16_t sub16(int16_t x, int16_t y)\n{\n    return x - y;\n}\nstatic inline int32_t sub32(int32_t x, int32_t y)\n{\n    return x - y;\n}\nstatic inline int64_t sub64(int64_t x, int64_t y)\n{\n    return x - y;\n}\nstatic inline int8_t mul8(int8_t x, int8_t y)\n{\n    return x * y;\n}\nstatic inline int16_t mul16(int16_t x, int16_t y)\n{\n    return x * y;\n}\nstatic inline int32_t mul32(int32_t x, int32_t y)\n{\n    return x * y;\n}\nstatic inline int64_t mul64(int64_t x, int64_t y)\n{\n    return x * y;\n}\nstatic inline uint8_t udiv8(uint8_t x, uint8_t y)\n{\n    return x / y;\n}\nstatic inline uint16_t udiv16(uint16_t x, uint16_t y)\n{\n    return x / y;\n}\nstatic inline uint32_t udiv32(uint32_t x, uint32_t y)\n{\n    return x / y;\n}\nstatic inline uint64_t udiv64(uint64_t x, uint64_t y)\n{\n    return x / y;\n}\nstatic inline uint8_t umod8(uint8_t x, uint8_t y)\n{\n    return x % y;\n}\nstatic inline uint16_t umod16(uint16_t x, uint16_t y)\n{\n    return x % y;\n}\nstatic inline uint32_t umod32(uint32_t x, uint32_t y)\n{\n    return x % y;\n}\nstatic inline uint64_t umod64(uint64_t x, uint64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t sdiv8(int8_t x, int8_t y)\n{\n    int8_t q = x / y;\n    int8_t r = x % y;\n    \n    return q - ((",
            "r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int16_t sdiv16(int16_t x, int16_t y)\n{\n    int16_t q = x / y;\n    int16_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int32_t sdiv32(int32_t x, int32_t y)\n{\n    int32_t q = x / y;\n    int32_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int64_t sdiv64(int64_t x, int64_t y)\n{\n    int64_t q = x / y;\n    int64_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int8_t smod8(int8_t x, int8_t y)\n{\n    int8_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int16_t smod16(int16_t x, int16_t y)\n{\n    int16_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int32_t smod32(int32_t x, int32_t y)\n{\n    int32_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int64_t smod64(int64_t x, int64_t y)\n{\n    int64_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int8_t squot8(int8_t x, int8_t y)\n{\n    return x / y;\n}\nstatic inline int16_t squot16(int16_t x, int16_t y)\n{\n    return x / y;\n}\nstatic inline int32_t squot32(int32_t x, int32_t y)\n{\n    return x / y;\n}\nstatic inline int64_t squot64(int64_t x, int64_t y)\n{\n    return x / y;\n}\nstatic inline int8_t srem8(int8_t x, int8_t y)\n{\n    return x % y;\n}\nstatic inline int16_t srem16(int16_t x, int16_t y)\n{\n    return x % y;\n}\nstatic inline int32_t srem32(int32_t x, int32_t y)\n{\n    return x % y;\n}\nstatic inline int64_t srem64(int64_t x, int64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t smin8(int8_t x, int8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int16_t smin16(int16_t x, int16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int32_t smin32(int32_t x, int32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int64_t smin64(int64_t x, int64_t",
            " y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint8_t umin8(uint8_t x, uint8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint16_t umin16(uint16_t x, uint16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint32_t umin32(uint32_t x, uint32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint64_t umin64(uint64_t x, uint64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int8_t smax8(int8_t x, int8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int16_t smax16(int16_t x, int16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int32_t smax32(int32_t x, int32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int64_t smax64(int64_t x, int64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t umax8(uint8_t x, uint8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint16_t umax16(uint16_t x, uint16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint32_t umax32(uint32_t x, uint32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint64_t umax64(uint64_t x, uint64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t shl8(uint8_t x, uint8_t y)\n{\n    return x << y;\n}\nstatic inline uint16_t shl16(uint16_t x, uint16_t y)\n{\n    return x << y;\n}\nstatic inline uint32_t shl32(uint32_t x, uint32_t y)\n{\n    return x << y;\n}\nstatic inline uint64_t shl64(uint64_t x, uint64_t y)\n{\n    return x << y;\n}\nstatic inline uint8_t lshr8(uint8_t x, uint8_t y)\n{\n    return x >> y;\n}\nstatic inline uint16_t lshr16(uint16_t x, uint16_t y)\n{\n    return x >> y;\n}\nstatic inline uint32_t lshr32(uint32_t x, uint32_t y)\n{\n    return x >> y;\n}\nstatic inline uint64_t lshr64(uint64_t x, uint64_t y)\n{\n    return x >> y;\n}\nstatic inline int8_t ashr8(int8_t x, int8_t y)\n{\n    return x >> y;\n}\nstatic inline int16_t ashr16(int16_t x, int16_t y)\n{\n    return x >> y;\n}\nstatic inline int32_t ashr32(int32_t x, int32_t y)\n{\n    return x >> y;\n}\nstatic inline int64_t ashr64(int64_t x, int64_t y)\n{\n    return x >> y;\n}\nstatic inline uint8_t and8(uint8_t x, uint8_t y)\n{\n    return x & y;\n}\nstatic inline u",
            "int16_t and16(uint16_t x, uint16_t y)\n{\n    return x & y;\n}\nstatic inline uint32_t and32(uint32_t x, uint32_t y)\n{\n    return x & y;\n}\nstatic inline uint64_t and64(uint64_t x, uint64_t y)\n{\n    return x & y;\n}\nstatic inline uint8_t or8(uint8_t x, uint8_t y)\n{\n    return x | y;\n}\nstatic inline uint16_t or16(uint16_t x, uint16_t y)\n{\n    return x | y;\n}\nstatic inline uint32_t or32(uint32_t x, uint32_t y)\n{\n    return x | y;\n}\nstatic inline uint64_t or64(uint64_t x, uint64_t y)\n{\n    return x | y;\n}\nstatic inline uint8_t xor8(uint8_t x, uint8_t y)\n{\n    return x ^ y;\n}\nstatic inline uint16_t xor16(uint16_t x, uint16_t y)\n{\n    return x ^ y;\n}\nstatic inline uint32_t xor32(uint32_t x, uint32_t y)\n{\n    return x ^ y;\n}\nstatic inline uint64_t xor64(uint64_t x, uint64_t y)\n{\n    return x ^ y;\n}\nstatic inline char ult8(uint8_t x, uint8_t y)\n{\n    return x < y;\n}\nstatic inline char ult16(uint16_t x, uint16_t y)\n{\n    return x < y;\n}\nstatic inline char ult32(uint32_t x, uint32_t y)\n{\n    return x < y;\n}\nstatic inline char ult64(uint64_t x, uint64_t y)\n{\n    return x < y;\n}\nstatic inline char ule8(uint8_t x, uint8_t y)\n{\n    return x <= y;\n}\nstatic inline char ule16(uint16_t x, uint16_t y)\n{\n    return x <= y;\n}\nstatic inline char ule32(uint32_t x, uint32_t y)\n{\n    return x <= y;\n}\nstatic inline char ule64(uint64_t x, uint64_t y)\n{\n    return x <= y;\n}\nstatic inline char slt8(int8_t x, int8_t y)\n{\n    return x < y;\n}\nstatic inline char slt16(int16_t x, int16_t y)\n{\n    return x < y;\n}\nstatic inline char slt32(int32_t x, int32_t y)\n{\n    return x < y;\n}\nstatic inline char slt64(int64_t x, int64_t y)\n{\n    return x < y;\n}\nstatic inline char sle8(int8_t x, int8_t y)\n{\n    return x <= y;\n}\nstatic inline char sle16(int16_t x, int16_t y)\n{\n    return x <= y;\n}\nstatic inline char sle32(int32_t x, int32_t y)\n{\n    return x <= y;\n}\nstatic inline char sle64(int64_t x, int64_t y)\n{\n    return x <= y;\n}\nstatic inline int8_t pow8(int8_t x, int8_t y)\n{\n    int8_t res = 1, rem = y;\n    \n    ",
            "while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int16_t pow16(int16_t x, int16_t y)\n{\n    int16_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int32_t pow32(int32_t x, int32_t y)\n{\n    int32_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int64_t pow64(int64_t x, int64_t y)\n{\n    int64_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int8_t sext_i8_i8(int8_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i8_i16(int8_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i8_i32(int8_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i8_i64(int8_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i16_i8(int16_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i16_i16(int16_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i16_i32(int16_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i16_i64(int16_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i32_i8(int32_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i32_i16(int32_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i32_i32(int32_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i32_i64(int32_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i64_i8(int64_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i64_i16(int64_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i64_i32(int64_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i64_i64(int64_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i8_i8(uint8_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i8_i16(uint8_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i8_i32(uint8_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i8_i64(uint8_t x)",
            "\n{\n    return x;\n}\nstatic inline uint8_t zext_i16_i8(uint16_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i16_i16(uint16_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i16_i32(uint16_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i16_i64(uint16_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i32_i8(uint32_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i32_i16(uint32_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i32_i32(uint32_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i32_i64(uint32_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i64_i8(uint64_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i64_i16(uint64_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i64_i32(uint64_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i64_i64(uint64_t x)\n{\n    return x;\n}\nstatic inline float fdiv32(float x, float y)\n{\n    return x / y;\n}\nstatic inline float fadd32(float x, float y)\n{\n    return x + y;\n}\nstatic inline float fsub32(float x, float y)\n{\n    return x - y;\n}\nstatic inline float fmul32(float x, float y)\n{\n    return x * y;\n}\nstatic inline float fmin32(float x, float y)\n{\n    return x < y ? x : y;\n}\nstatic inline float fmax32(float x, float y)\n{\n    return x < y ? y : x;\n}\nstatic inline float fpow32(float x, float y)\n{\n    return pow(x, y);\n}\nstatic inline char cmplt32(float x, float y)\n{\n    return x < y;\n}\nstatic inline char cmple32(float x, float y)\n{\n    return x <= y;\n}\nstatic inline float sitofp_i8_f32(int8_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i16_f32(int16_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i32_f32(int32_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i64_f32(int64_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i8_f32(uint8_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i16_f32(uint16_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i32_f32(uint32_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i64_f32(uint64_t x)\n{\n    return x;\n}\nstatic inline int8_t fptosi_f32_i8(float x)\n{\n    return x;",
            "\n}\nstatic inline int16_t fptosi_f32_i16(float x)\n{\n    return x;\n}\nstatic inline int32_t fptosi_f32_i32(float x)\n{\n    return x;\n}\nstatic inline int64_t fptosi_f32_i64(float x)\n{\n    return x;\n}\nstatic inline uint8_t fptoui_f32_i8(float x)\n{\n    return x;\n}\nstatic inline uint16_t fptoui_f32_i16(float x)\n{\n    return x;\n}\nstatic inline uint32_t fptoui_f32_i32(float x)\n{\n    return x;\n}\nstatic inline uint64_t fptoui_f32_i64(float x)\n{\n    return x;\n}\nstatic inline float futrts_log32(float x)\n{\n    return log(x);\n}\nstatic inline float futrts_sqrt32(float x)\n{\n    return sqrt(x);\n}\nstatic inline float futrts_exp32(float x)\n{\n    return exp(x);\n}\nstatic inline float futrts_cos32(float x)\n{\n    return cos(x);\n}\nstatic inline float futrts_sin32(float x)\n{\n    return sin(x);\n}\nstatic inline float futrts_acos32(float x)\n{\n    return acos(x);\n}\nstatic inline float futrts_asin32(float x)\n{\n    return asin(x);\n}\nstatic inline float futrts_atan32(float x)\n{\n    return atan(x);\n}\nstatic inline float futrts_atan2_32(float x, float y)\n{\n    return atan2(x, y);\n}\nstatic inline char futrts_isnan32(float x)\n{\n    return isnan(x);\n}\nstatic inline char futrts_isinf32(float x)\n{\n    return isinf(x);\n}\nstatic inline int32_t futrts_to_bits32(float x)\n{\n    union {\n        float f;\n        int32_t t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\nstatic inline float futrts_from_bits32(int32_t x)\n{\n    union {\n        int32_t f;\n        float t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\n#define group_sizze_3312 (DEFAULT_GROUP_SIZE)\n#define group_sizze_3312 (DEFAULT_GROUP_SIZE)\n__kernel void chunked_reduce_kernel_3328(__local volatile\n                                         int64_t *mem_aligned_0,\n                                         __local volatile\n                                         int64_t *mem_aligned_1,\n                                         int32_t sizze_3261,\n                                         int32_t num_threads_3319,\n                                         int32_",
            "t per_thread_elements_3322,\n                                         int32_t per_chunk_3404,\n                                         int32_t per_chunk_3416, __global\n                                         unsigned char *mem_3437, __global\n                                         unsigned char *mem_3448, __global\n                                         unsigned char *mem_3457, __global\n                                         unsigned char *mem_3460)\n{\n    __local volatile char *restrict mem_3451 = mem_aligned_0;\n    __local volatile char *restrict mem_3454 = mem_aligned_1;\n    int32_t wave_sizze_3479;\n    int32_t group_sizze_3480;\n    char thread_active_3481;\n    int32_t global_tid_3328;\n    int32_t local_tid_3329;\n    int32_t group_id_3330;\n    \n    global_tid_3328 = get_global_id(0);\n    local_tid_3329 = get_local_id(0);\n    group_sizze_3480 = get_local_size(0);\n    wave_sizze_3479 = LOCKSTEP_WIDTH;\n    group_id_3330 = get_group_id(0);\n    thread_active_3481 = 1;\n    \n    int32_t chunk_sizze_3338;\n    int32_t starting_point_3482 = global_tid_3328 * per_thread_elements_3322;\n    int32_t remaining_elements_3483 = sizze_3261 - starting_point_3482;\n    \n    if (sle32(remaining_elements_3483, 0) || sle32(sizze_3261,\n                                                   starting_point_3482)) {\n        chunk_sizze_3338 = 0;\n    } else {\n        if (slt32(sizze_3261, (global_tid_3328 + 1) *\n                  per_thread_elements_3322)) {\n            chunk_sizze_3338 = sizze_3261 - global_tid_3328 *\n                per_thread_elements_3322;\n        } else {\n            chunk_sizze_3338 = per_thread_elements_3322;\n        }\n    }\n    \n    int32_t slice_offset_3339;\n    \n    if (thread_active_3481) {\n        slice_offset_3339 = global_tid_3328 * per_thread_elements_3322;\n    }\n    \n    int32_t res_3345;\n    int32_t res_3346;\n    int32_t final_result_3368;\n    int32_t final_result_3369;\n    int32_t acc_3349;\n    int32_t acc_3350;\n    \n    acc_3349 = 0;\n    acc_3350 = -1;\n    ",
            "\n    int32_t groupstream_mapaccum_dummy_chunk_sizze_3347 = 1;\n    \n    if (thread_active_3481) {\n        for (int32_t i_3348 = 0; i_3348 < chunk_sizze_3338; i_3348++) {\n            int32_t binop_param_x_3354 = *(__global\n                                           int32_t *) &mem_3437[((slice_offset_3339 +\n                                                                  i_3348 -\n                                                                  squot32(slice_offset_3339 +\n                                                                          i_3348,\n                                                                          per_chunk_3404) *\n                                                                  per_chunk_3404) *\n                                                                 num_threads_3319 +\n                                                                 squot32(slice_offset_3339 +\n                                                                         i_3348,\n                                                                         per_chunk_3404)) *\n                                                                4];\n            int32_t binop_param_y_3355 = *(__global\n                                           int32_t *) &mem_3448[((slice_offset_3339 +\n                                                                  i_3348 -\n                                                                  squot32(slice_offset_3339 +\n                                                                          i_3348,\n                                                                          per_chunk_3416) *\n                                                                  per_chunk_3416) *\n                                                                 num_threads_3319 +\n                                                                 squot32(slice_offset_3339 +\n                                                                         i_3348,\n                         ",
            "                                                per_chunk_3416)) *\n                                                                4];\n            int32_t convop_x_3394 = slice_offset_3339 + i_3348;\n            int32_t res_3359 = binop_param_x_3354 - binop_param_y_3355;\n            int32_t res_3360 = abs(res_3359);\n            char cond_3361 = slt32(res_3360, acc_3349);\n            int32_t res_3362;\n            \n            if (cond_3361) {\n                res_3362 = acc_3349;\n            } else {\n                res_3362 = res_3360;\n            }\n            \n            int32_t res_3363;\n            \n            if (cond_3361) {\n                res_3363 = acc_3350;\n            } else {\n                res_3363 = convop_x_3394;\n            }\n            acc_3349 = res_3362;\n            acc_3350 = res_3363;\n        }\n    }\n    res_3345 = acc_3349;\n    res_3346 = acc_3350;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_3329, group_sizze_3312) && 1) {\n        *(__local int32_t *) &mem_3451[local_tid_3329 * 4] = res_3345;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_3329, group_sizze_3312) && 1) {\n        *(__local int32_t *) &mem_3454[local_tid_3329 * 4] = res_3346;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t skip_waves_3484;\n    int32_t my_index_3370;\n    int32_t other_offset_3371;\n    int32_t not_curried_3372;\n    int32_t not_curried_3373;\n    int32_t not_curried_3374;\n    int32_t not_curried_3375;\n    \n    my_index_3370 = local_tid_3329;\n    other_offset_3371 = 0;\n    not_curried_3372 = *(__local int32_t *) &mem_3451[(local_tid_3329 +\n                                                       other_offset_3371) * 4];\n    not_curried_3373 = *(__local int32_t *) &mem_3454[(local_tid_3329 +\n                                                       other_offset_3371) * 4];\n    other_offset_3371 = 1;\n    while (slt32(other_offset_3371, wave_sizze_3479)) {\n        if (slt32(local_tid_3329 + other_offse",
            "t_3371, group_sizze_3312) &&\n            ((local_tid_3329 - squot32(local_tid_3329, wave_sizze_3479) *\n              wave_sizze_3479) & (2 * other_offset_3371 - 1)) == 0) {\n            // read array element\n            {\n                not_curried_3374 = *(volatile __local\n                                     int32_t *) &mem_3451[(local_tid_3329 +\n                                                           other_offset_3371) *\n                                                          4];\n                not_curried_3375 = *(volatile __local\n                                     int32_t *) &mem_3454[(local_tid_3329 +\n                                                           other_offset_3371) *\n                                                          4];\n            }\n            \n            char cond_3376;\n            int32_t res_3377;\n            int32_t res_3378;\n            \n            if (thread_active_3481) {\n                cond_3376 = slt32(not_curried_3374, not_curried_3372);\n                if (cond_3376) {\n                    res_3377 = not_curried_3372;\n                } else {\n                    res_3377 = not_curried_3374;\n                }\n                if (cond_3376) {\n                    res_3378 = not_curried_3373;\n                } else {\n                    res_3378 = not_curried_3375;\n                }\n            }\n            not_curried_3372 = res_3377;\n            not_curried_3373 = res_3378;\n            *(volatile __local int32_t *) &mem_3451[local_tid_3329 * 4] =\n                not_curried_3372;\n            *(volatile __local int32_t *) &mem_3454[local_tid_3329 * 4] =\n                not_curried_3373;\n        }\n        other_offset_3371 *= 2;\n    }\n    skip_waves_3484 = 1;\n    while (slt32(skip_waves_3484, squot32(group_sizze_3480 + wave_sizze_3479 -\n                                          1, wave_sizze_3479))) {\n        barrier(CLK_LOCAL_MEM_FENCE);\n        other_offset_3371 = skip_waves_3484 * wave_sizze_3479;\n        if ((local_",
            "tid_3329 - squot32(local_tid_3329, wave_sizze_3479) *\n             wave_sizze_3479) == 0 && (squot32(local_tid_3329,\n                                               wave_sizze_3479) & (2 *\n                                                                   skip_waves_3484 -\n                                                                   1)) == 0) {\n            // read array element\n            {\n                not_curried_3374 = *(__local\n                                     int32_t *) &mem_3451[(local_tid_3329 +\n                                                           other_offset_3371) *\n                                                          4];\n                not_curried_3375 = *(__local\n                                     int32_t *) &mem_3454[(local_tid_3329 +\n                                                           other_offset_3371) *\n                                                          4];\n            }\n            \n            char cond_3376;\n            int32_t res_3377;\n            int32_t res_3378;\n            \n            if (thread_active_3481) {\n                cond_3376 = slt32(not_curried_3374, not_curried_3372);\n                if (cond_3376) {\n                    res_3377 = not_curried_3372;\n                } else {\n                    res_3377 = not_curried_3374;\n                }\n                if (cond_3376) {\n                    res_3378 = not_curried_3373;\n                } else {\n                    res_3378 = not_curried_3375;\n                }\n            }\n            not_curried_3372 = res_3377;\n            not_curried_3373 = res_3378;\n            *(__local int32_t *) &mem_3451[local_tid_3329 * 4] =\n                not_curried_3372;\n            *(__local int32_t *) &mem_3454[local_tid_3329 * 4] =\n                not_curried_3373;\n        }\n        skip_waves_3484 *= 2;\n    }\n    final_result_3368 = not_curried_3372;\n    final_result_3369 = not_curried_3373;\n    if (local_tid_3329 == 0) {\n        *(__global int32_t *) &mem",
            "_3457[group_id_3330 * 4] = final_result_3368;\n    }\n    if (local_tid_3329 == 0) {\n        *(__global int32_t *) &mem_3460[group_id_3330 * 4] = final_result_3369;\n    }\n}\n__kernel void fut_kernel_map_transpose_i32(__global int32_t *odata,\n                                           uint odata_offset, __global\n                                           int32_t *idata, uint idata_offset,\n                                           uint width, uint height,\n                                           uint input_size, uint output_size,\n                                           __local int32_t *block)\n{\n    uint x_index;\n    uint y_index;\n    uint our_array_offset;\n    \n    // Adjust the input and output arrays with the basic offset.\n    odata += odata_offset / sizeof(int32_t);\n    idata += idata_offset / sizeof(int32_t);\n    // Adjust the input and output arrays for the third dimension.\n    our_array_offset = get_global_id(2) * width * height;\n    odata += our_array_offset;\n    idata += our_array_offset;\n    // read the matrix tile into shared memory\n    x_index = get_global_id(0);\n    y_index = get_global_id(1);\n    \n    uint index_in = y_index * width + x_index;\n    \n    if ((x_index < width && y_index < height) && index_in < input_size)\n        block[get_local_id(1) * (FUT_BLOCK_DIM + 1) + get_local_id(0)] =\n            idata[index_in];\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // Scatter the transposed matrix tile to global memory.\n    x_index = get_group_id(1) * FUT_BLOCK_DIM + get_local_id(0);\n    y_index = get_group_id(0) * FUT_BLOCK_DIM + get_local_id(1);\n    \n    uint index_out = y_index * height + x_index;\n    \n    if ((x_index < height && y_index < width) && index_out < output_size)\n        odata[index_out] = block[get_local_id(0) * (FUT_BLOCK_DIM + 1) +\n                                 get_local_id(1)];\n}\n__kernel void fut_kernel_map_transpose_lowheight_i32(__global int32_t *odata,\n                                                     uint odata_offset, __global\n   ",
            "                                                  int32_t *idata,\n                                                     uint idata_offset,\n                                                     uint width, uint height,\n                                                     uint input_size,\n                                                     uint output_size,\n                                                     uint mulx, __local\n                                                     int32_t *block)\n{\n    uint x_index;\n    uint y_index;\n    uint our_array_offset;\n    \n    // Adjust the input and output arrays with the basic offset.\n    odata += odata_offset / sizeof(int32_t);\n    idata += idata_offset / sizeof(int32_t);\n    // Adjust the input and output arrays for the third dimension.\n    our_array_offset = get_global_id(2) * width * height;\n    odata += our_array_offset;\n    idata += our_array_offset;\n    // read the matrix tile into shared memory\n    x_index = get_group_id(0) * FUT_BLOCK_DIM * mulx + get_local_id(0) +\n        get_local_id(1) % mulx * FUT_BLOCK_DIM;\n    y_index = get_group_id(1) * FUT_BLOCK_DIM + get_local_id(1) / mulx;\n    \n    uint index_in = y_index * width + x_index;\n    \n    if ((x_index < width && y_index < height) && index_in < input_size)\n        block[get_local_id(1) * (FUT_BLOCK_DIM + 1) + get_local_id(0)] =\n            idata[index_in];\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // Scatter the transposed matrix tile to global memory.\n    x_index = get_group_id(1) * FUT_BLOCK_DIM + get_local_id(0) / mulx;\n    y_index = get_group_id(0) * FUT_BLOCK_DIM * mulx + get_local_id(1) +\n        get_local_id(0) % mulx * FUT_BLOCK_DIM;\n    \n    uint index_out = y_index * height + x_index;\n    \n    if ((x_index < height && y_index < width) && index_out < output_size)\n        odata[index_out] = block[get_local_id(0) * (FUT_BLOCK_DIM + 1) +\n                                 get_local_id(1)];\n}\n__kernel void fut_kernel_map_transpose_lowwidth_i32(__global int32_t *oda",
            "ta,\n                                                    uint odata_offset, __global\n                                                    int32_t *idata,\n                                                    uint idata_offset,\n                                                    uint width, uint height,\n                                                    uint input_size,\n                                                    uint output_size, uint muly,\n                                                    __local int32_t *block)\n{\n    uint x_index;\n    uint y_index;\n    uint our_array_offset;\n    \n    // Adjust the input and output arrays with the basic offset.\n    odata += odata_offset / sizeof(int32_t);\n    idata += idata_offset / sizeof(int32_t);\n    // Adjust the input and output arrays for the third dimension.\n    our_array_offset = get_global_id(2) * width * height;\n    odata += our_array_offset;\n    idata += our_array_offset;\n    // read the matrix tile into shared memory\n    x_index = get_group_id(0) * FUT_BLOCK_DIM + get_local_id(0) / muly;\n    y_index = get_group_id(1) * FUT_BLOCK_DIM * muly + get_local_id(1) +\n        get_local_id(0) % muly * FUT_BLOCK_DIM;\n    \n    uint index_in = y_index * width + x_index;\n    \n    if ((x_index < width && y_index < height) && index_in < input_size)\n        block[get_local_id(1) * (FUT_BLOCK_DIM + 1) + get_local_id(0)] =\n            idata[index_in];\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // Scatter the transposed matrix tile to global memory.\n    x_index = get_group_id(1) * FUT_BLOCK_DIM * muly + get_local_id(0) +\n        get_local_id(1) % muly * FUT_BLOCK_DIM;\n    y_index = get_group_id(0) * FUT_BLOCK_DIM + get_local_id(1) / muly;\n    \n    uint index_out = y_index * height + x_index;\n    \n    if ((x_index < height && y_index < width) && index_out < output_size)\n        odata[index_out] = block[get_local_id(0) * (FUT_BLOCK_DIM + 1) +\n                                 get_local_id(1)];\n}\n__kernel void reduce_kernel_3382(__local vola",
            "tile int64_t *mem_aligned_0,\n                                 __local volatile int64_t *mem_aligned_1,\n                                 int32_t num_groups_3318, __global\n                                 unsigned char *mem_3457, __global\n                                 unsigned char *mem_3460, __global\n                                 unsigned char *mem_3469, __global\n                                 unsigned char *mem_3472)\n{\n    __local volatile char *restrict mem_3463 = mem_aligned_0;\n    __local volatile char *restrict mem_3466 = mem_aligned_1;\n    int32_t wave_sizze_3485;\n    int32_t group_sizze_3486;\n    char thread_active_3487;\n    int32_t global_tid_3382;\n    int32_t local_tid_3383;\n    int32_t group_id_3384;\n    \n    global_tid_3382 = get_global_id(0);\n    local_tid_3383 = get_local_id(0);\n    group_sizze_3486 = get_local_size(0);\n    wave_sizze_3485 = LOCKSTEP_WIDTH;\n    group_id_3384 = get_group_id(0);\n    thread_active_3487 = 1;\n    \n    char in_bounds_3385;\n    \n    if (thread_active_3487) {\n        in_bounds_3385 = slt32(local_tid_3383, num_groups_3318);\n    }\n    \n    int32_t final_result_3392;\n    int32_t final_result_3393;\n    \n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_3383, group_sizze_3312) && 1) {\n        int32_t elem_3387;\n        \n        if (in_bounds_3385) {\n            int32_t x_3386 = *(__global int32_t *) &mem_3457[global_tid_3382 *\n                                                             4];\n            \n            elem_3387 = x_3386;\n        } else {\n            elem_3387 = 0;\n        }\n        \n        int32_t elem_3389;\n        \n        if (in_bounds_3385) {\n            int32_t x_3388 = *(__global int32_t *) &mem_3460[global_tid_3382 *\n                                                             4];\n            \n            elem_3389 = x_3388;\n        } else {\n            elem_3389 = -1;\n        }\n        *(__local int32_t *) &mem_3463[local_tid_3383 * 4] = elem_3387;\n        *(__local int32_t *) &mem_3466[local_ti",
            "d_3383 * 4] = elem_3389;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t skip_waves_3488;\n    int32_t not_curried_3279;\n    int32_t not_curried_3280;\n    int32_t not_curried_3281;\n    int32_t not_curried_3282;\n    int32_t my_index_3326;\n    int32_t other_offset_3327;\n    \n    my_index_3326 = local_tid_3383;\n    other_offset_3327 = 0;\n    not_curried_3279 = *(__local int32_t *) &mem_3463[(local_tid_3383 +\n                                                       other_offset_3327) * 4];\n    not_curried_3280 = *(__local int32_t *) &mem_3466[(local_tid_3383 +\n                                                       other_offset_3327) * 4];\n    other_offset_3327 = 1;\n    while (slt32(other_offset_3327, wave_sizze_3485)) {\n        if (slt32(local_tid_3383 + other_offset_3327, group_sizze_3312) &&\n            ((local_tid_3383 - squot32(local_tid_3383, wave_sizze_3485) *\n              wave_sizze_3485) & (2 * other_offset_3327 - 1)) == 0) {\n            // read array element\n            {\n                not_curried_3281 = *(volatile __local\n                                     int32_t *) &mem_3463[(local_tid_3383 +\n                                                           other_offset_3327) *\n                                                          4];\n                not_curried_3282 = *(volatile __local\n                                     int32_t *) &mem_3466[(local_tid_3383 +\n                                                           other_offset_3327) *\n                                                          4];\n            }\n            \n            char cond_3283;\n            int32_t res_3284;\n            int32_t res_3285;\n            \n            if (thread_active_3487) {\n                cond_3283 = slt32(not_curried_3281, not_curried_3279);\n                if (cond_3283) {\n                    res_3284 = not_curried_3279;\n                } else {\n                    res_3284 = not_curried_3281;\n                }\n                if (cond_3283) {\n             ",
            "       res_3285 = not_curried_3280;\n                } else {\n                    res_3285 = not_curried_3282;\n                }\n            }\n            not_curried_3279 = res_3284;\n            not_curried_3280 = res_3285;\n            *(volatile __local int32_t *) &mem_3463[local_tid_3383 * 4] =\n                not_curried_3279;\n            *(volatile __local int32_t *) &mem_3466[local_tid_3383 * 4] =\n                not_curried_3280;\n        }\n        other_offset_3327 *= 2;\n    }\n    skip_waves_3488 = 1;\n    while (slt32(skip_waves_3488, squot32(group_sizze_3486 + wave_sizze_3485 -\n                                          1, wave_sizze_3485))) {\n        barrier(CLK_LOCAL_MEM_FENCE);\n        other_offset_3327 = skip_waves_3488 * wave_sizze_3485;\n        if ((local_tid_3383 - squot32(local_tid_3383, wave_sizze_3485) *\n             wave_sizze_3485) == 0 && (squot32(local_tid_3383,\n                                               wave_sizze_3485) & (2 *\n                                                                   skip_waves_3488 -\n                                                                   1)) == 0) {\n            // read array element\n            {\n                not_curried_3281 = *(__local\n                                     int32_t *) &mem_3463[(local_tid_3383 +\n                                                           other_offset_3327) *\n                                                          4];\n                not_curried_3282 = *(__local\n                                     int32_t *) &mem_3466[(local_tid_3383 +\n                                                           other_offset_3327) *\n                                                          4];\n            }\n            \n            char cond_3283;\n            int32_t res_3284;\n            int32_t res_3285;\n            \n            if (thread_active_3487) {\n                cond_3283 = slt32(not_curried_3281, not_curried_3279);\n                if (cond_3283) {\n                    res_32",
            "84 = not_curried_3279;\n                } else {\n                    res_3284 = not_curried_3281;\n                }\n                if (cond_3283) {\n                    res_3285 = not_curried_3280;\n                } else {\n                    res_3285 = not_curried_3282;\n                }\n            }\n            not_curried_3279 = res_3284;\n            not_curried_3280 = res_3285;\n            *(__local int32_t *) &mem_3463[local_tid_3383 * 4] =\n                not_curried_3279;\n            *(__local int32_t *) &mem_3466[local_tid_3383 * 4] =\n                not_curried_3280;\n        }\n        skip_waves_3488 *= 2;\n    }\n    final_result_3392 = not_curried_3279;\n    final_result_3393 = not_curried_3280;\n    if (local_tid_3383 == 0) {\n        *(__global int32_t *) &mem_3469[group_id_3384 * 4] = final_result_3392;\n    }\n    if (local_tid_3383 == 0) {\n        *(__global int32_t *) &mem_3472[group_id_3384 * 4] = final_result_3393;\n    }\n}\n",
            NULL};
struct memblock_device {
    int *references;
    cl_mem mem;
    int64_t size;
} ;
struct memblock_local {
    int *references;
    unsigned char mem;
    int64_t size;
} ;
struct memblock {
    int *references;
    char *mem;
    int64_t size;
} ;
struct futhark_context_config {
    struct opencl_config opencl;
} ;
struct futhark_context_config *futhark_context_config_new()
{
    struct futhark_context_config *cfg =
                                  malloc(sizeof(struct futhark_context_config));
    
    if (cfg == NULL)
        return NULL;
    opencl_config_init(&cfg->opencl);
    cfg->opencl.transpose_block_dim = 16;
    return cfg;
}
void futhark_context_config_free(struct futhark_context_config *cfg)
{
    free(cfg);
}
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag)
{
    cfg->opencl.debugging = flag;
}
void futhark_context_config_set_device(struct futhark_context_config *cfg, const
                                       char *s)
{
    set_preferred_device(&cfg->opencl, s);
}
void futhark_context_config_set_platform(struct futhark_context_config *cfg,
                                         const char *s)
{
    set_preferred_platform(&cfg->opencl, s);
}
void futhark_context_config_dump_program_to(struct futhark_context_config *cfg,
                                            const char *path)
{
    cfg->opencl.dump_program_to = path;
}
void futhark_context_config_load_program_from(struct futhark_context_config *cfg,
                                              const char *path)
{
    cfg->opencl.load_program_from = path;
}
void futhark_context_config_set_group_size(struct futhark_context_config *cfg,
                                           int size)
{
    cfg->opencl.group_size = size;
}
void futhark_context_config_set_num_groups(struct futhark_context_config *cfg,
                                           int num)
{
    cfg->opencl.num_groups = num;
}
struct futhark_context {
    int detail_memory;
    int debugging;
    int64_t peak_mem_usage_device;
    int64_t cur_mem_usage_device;
    int64_t peak_mem_usage_local;
    int64_t cur_mem_usage_local;
    int64_t peak_mem_usage_default;
    int64_t cur_mem_usage_default;
    int total_runs;
    int total_runtime;
    cl_kernel chunked_reduce_kernel_3328;
    int chunked_reduce_kernel_3328_total_runtime;
    int chunked_reduce_kernel_3328_runs;
    cl_kernel fut_kernel_map_transpose_i32;
    int fut_kernel_map_transpose_i32_total_runtime;
    int fut_kernel_map_transpose_i32_runs;
    cl_kernel fut_kernel_map_transpose_lowheight_i32;
    int fut_kernel_map_transpose_lowheight_i32_total_runtime;
    int fut_kernel_map_transpose_lowheight_i32_runs;
    cl_kernel fut_kernel_map_transpose_lowwidth_i32;
    int fut_kernel_map_transpose_lowwidth_i32_total_runtime;
    int fut_kernel_map_transpose_lowwidth_i32_runs;
    cl_kernel reduce_kernel_3382;
    int reduce_kernel_3382_total_runtime;
    int reduce_kernel_3382_runs;
    struct opencl_context opencl;
} ;
void setup_opencl_and_load_kernels(struct futhark_context *ctx)
{
    cl_int error;
    cl_program prog = setup_opencl(&ctx->opencl, opencl_program);
    
    {
        ctx->chunked_reduce_kernel_3328 = clCreateKernel(prog,
                                                         "chunked_reduce_kernel_3328",
                                                         &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n",
                    "chunked_reduce_kernel_3328");
    }
    {
        ctx->fut_kernel_map_transpose_i32 = clCreateKernel(prog,
                                                           "fut_kernel_map_transpose_i32",
                                                           &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n",
                    "fut_kernel_map_transpose_i32");
    }
    {
        ctx->fut_kernel_map_transpose_lowheight_i32 = clCreateKernel(prog,
                                                                     "fut_kernel_map_transpose_lowheight_i32",
                                                                     &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n",
                    "fut_kernel_map_transpose_lowheight_i32");
    }
    {
        ctx->fut_kernel_map_transpose_lowwidth_i32 = clCreateKernel(prog,
                                                                    "fut_kernel_map_transpose_lowwidth_i32",
                                                                    &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n",
                    "fut_kernel_map_transpose_lowwidth_i32");
    }
    {
        ctx->reduce_kernel_3382 = clCreateKernel(prog, "reduce_kernel_3382",
                                                 &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "reduce_kernel_3382");
    }
}
void post_opencl_setup(struct opencl_context *ctx,
                       struct opencl_device_option *option)
{
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name,
                                            "NVIDIA CUDA") != NULL) &&
        option->device_type == CL_DEVICE_TYPE_GPU)
        ctx->lockstep_width = 32;
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name,
                                            "AMD Accelerated Parallel Processing") !=
         NULL) && option->device_type == CL_DEVICE_TYPE_GPU)
        ctx->lockstep_width = 64;
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name, "") !=
         NULL) && option->device_type == CL_DEVICE_TYPE_GPU)
        ctx->lockstep_width = 1;
    if ((ctx->cfg.num_groups == 0 && strstr(option->platform_name, "") !=
         NULL) && option->device_type == CL_DEVICE_TYPE_GPU)
        ctx->cfg.num_groups = 128;
    if ((ctx->cfg.group_size == 0 && strstr(option->platform_name, "") !=
         NULL) && option->device_type == CL_DEVICE_TYPE_GPU)
        ctx->cfg.group_size = 256;
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name, "") !=
         NULL) && option->device_type == CL_DEVICE_TYPE_CPU)
        ctx->lockstep_width = 1;
    if ((ctx->cfg.num_groups == 0 && strstr(option->platform_name, "") !=
         NULL) && option->device_type == CL_DEVICE_TYPE_CPU)
        clGetDeviceInfo(ctx->device, CL_DEVICE_MAX_COMPUTE_UNITS,
                        sizeof(ctx->cfg.num_groups), &ctx->cfg.num_groups,
                        NULL);
    if ((ctx->cfg.group_size == 0 && strstr(option->platform_name, "") !=
         NULL) && option->device_type == CL_DEVICE_TYPE_CPU)
        ctx->cfg.group_size = 32;
}
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg)
{
    struct futhark_context *ctx = malloc(sizeof(struct futhark_context));
    
    if (ctx == NULL)
        return NULL;
    ctx->detail_memory = cfg->opencl.debugging;
    ctx->debugging = cfg->opencl.debugging;
    ctx->opencl.cfg = cfg->opencl;
    ctx->peak_mem_usage_device = 0;
    ctx->cur_mem_usage_device = 0;
    ctx->peak_mem_usage_local = 0;
    ctx->cur_mem_usage_local = 0;
    ctx->peak_mem_usage_default = 0;
    ctx->cur_mem_usage_default = 0;
    ctx->total_runs = 0;
    ctx->total_runtime = 0;
    ctx->chunked_reduce_kernel_3328_total_runtime = 0;
    ctx->chunked_reduce_kernel_3328_runs = 0;
    ctx->fut_kernel_map_transpose_i32_total_runtime = 0;
    ctx->fut_kernel_map_transpose_i32_runs = 0;
    ctx->fut_kernel_map_transpose_lowheight_i32_total_runtime = 0;
    ctx->fut_kernel_map_transpose_lowheight_i32_runs = 0;
    ctx->fut_kernel_map_transpose_lowwidth_i32_total_runtime = 0;
    ctx->fut_kernel_map_transpose_lowwidth_i32_runs = 0;
    ctx->reduce_kernel_3382_total_runtime = 0;
    ctx->reduce_kernel_3382_runs = 0;
    setup_opencl_and_load_kernels(ctx);
    return ctx;
}
void futhark_context_free(struct futhark_context *ctx)
{
    free(ctx);
}
int futhark_context_sync(struct futhark_context *ctx)
{
    OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
    return 0;
}
static void memblock_unref_device(struct futhark_context *ctx,
                                  struct memblock_device *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (ctx->detail_memory)
            fprintf(stderr,
                    "Unreferencing block in space 'device': %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            ctx->cur_mem_usage_device -= block->size;
            OPENCL_SUCCEED(clReleaseMemObject(block->mem));
            free(block->references);
            block->references = NULL;
            if (ctx->detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, ctx->cur_mem_usage_device);
        }
    }
}
static void memblock_alloc_device(struct futhark_context *ctx,
                                  struct memblock_device *block, int32_t size)
{
    memblock_unref_device(ctx, block);
    
    cl_int clCreateBuffer_succeeded_3489;
    
    block->mem = clCreateBuffer(ctx->opencl.ctx, CL_MEM_READ_WRITE, size >
                                0 ? size : 1, NULL,
                                &clCreateBuffer_succeeded_3489);
    OPENCL_SUCCEED(clCreateBuffer_succeeded_3489);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    ctx->cur_mem_usage_device += size;
    if (ctx->detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in space 'device' (now allocated: %ld bytes)",
                size, ctx->cur_mem_usage_device);
    if (ctx->cur_mem_usage_device > ctx->peak_mem_usage_device) {
        ctx->peak_mem_usage_device = ctx->cur_mem_usage_device;
        if (ctx->detail_memory)
            fprintf(stderr, " (new peak).\n");
    } else if (ctx->detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set_device(struct futhark_context *ctx,
                                struct memblock_device *lhs,
                                struct memblock_device *rhs)
{
    memblock_unref_device(ctx, lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
static void memblock_unref_local(struct futhark_context *ctx,
                                 struct memblock_local *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (ctx->detail_memory)
            fprintf(stderr,
                    "Unreferencing block in space 'local': %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            ctx->cur_mem_usage_local -= block->size;
            free(block->references);
            block->references = NULL;
            if (ctx->detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, ctx->cur_mem_usage_local);
        }
    }
}
static void memblock_alloc_local(struct futhark_context *ctx,
                                 struct memblock_local *block, int32_t size)
{
    memblock_unref_local(ctx, block);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    ctx->cur_mem_usage_local += size;
    if (ctx->detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in space 'local' (now allocated: %ld bytes)",
                size, ctx->cur_mem_usage_local);
    if (ctx->cur_mem_usage_local > ctx->peak_mem_usage_local) {
        ctx->peak_mem_usage_local = ctx->cur_mem_usage_local;
        if (ctx->detail_memory)
            fprintf(stderr, " (new peak).\n");
    } else if (ctx->detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set_local(struct futhark_context *ctx,
                               struct memblock_local *lhs,
                               struct memblock_local *rhs)
{
    memblock_unref_local(ctx, lhs);
    (*rhs->references)++;
    *lhs = *rhs;
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
        fprintf(stderr, "Peak memory usage for space 'device': %ld bytes.\n",
                ctx->peak_mem_usage_device);
        fprintf(stderr, "Peak memory usage for space 'local': %ld bytes.\n",
                ctx->peak_mem_usage_local);
        fprintf(stderr, "Peak memory usage for default space: %ld bytes.\n",
                ctx->peak_mem_usage_default);
    }
    if (ctx->debugging) {
        fprintf(stderr,
                "Kernel chunked_reduce_kernel_3328             executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->chunked_reduce_kernel_3328_runs,
                (long) ctx->chunked_reduce_kernel_3328_total_runtime /
                (ctx->chunked_reduce_kernel_3328_runs !=
                 0 ? ctx->chunked_reduce_kernel_3328_runs : 1),
                (long) ctx->chunked_reduce_kernel_3328_total_runtime);
        ctx->total_runtime += ctx->chunked_reduce_kernel_3328_total_runtime;
        ctx->total_runs += ctx->chunked_reduce_kernel_3328_runs;
        fprintf(stderr,
                "Kernel fut_kernel_map_transpose_i32           executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->fut_kernel_map_transpose_i32_runs,
                (long) ctx->fut_kernel_map_transpose_i32_total_runtime /
                (ctx->fut_kernel_map_transpose_i32_runs !=
                 0 ? ctx->fut_kernel_map_transpose_i32_runs : 1),
                (long) ctx->fut_kernel_map_transpose_i32_total_runtime);
        ctx->total_runtime += ctx->fut_kernel_map_transpose_i32_total_runtime;
        ctx->total_runs += ctx->fut_kernel_map_transpose_i32_runs;
        fprintf(stderr,
                "Kernel fut_kernel_map_transpose_lowheight_i32 executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->fut_kernel_map_transpose_lowheight_i32_runs,
                (long) ctx->fut_kernel_map_transpose_lowheight_i32_total_runtime /
                (ctx->fut_kernel_map_transpose_lowheight_i32_runs !=
                 0 ? ctx->fut_kernel_map_transpose_lowheight_i32_runs : 1),
                (long) ctx->fut_kernel_map_transpose_lowheight_i32_total_runtime);
        ctx->total_runtime +=
            ctx->fut_kernel_map_transpose_lowheight_i32_total_runtime;
        ctx->total_runs += ctx->fut_kernel_map_transpose_lowheight_i32_runs;
        fprintf(stderr,
                "Kernel fut_kernel_map_transpose_lowwidth_i32  executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->fut_kernel_map_transpose_lowwidth_i32_runs,
                (long) ctx->fut_kernel_map_transpose_lowwidth_i32_total_runtime /
                (ctx->fut_kernel_map_transpose_lowwidth_i32_runs !=
                 0 ? ctx->fut_kernel_map_transpose_lowwidth_i32_runs : 1),
                (long) ctx->fut_kernel_map_transpose_lowwidth_i32_total_runtime);
        ctx->total_runtime +=
            ctx->fut_kernel_map_transpose_lowwidth_i32_total_runtime;
        ctx->total_runs += ctx->fut_kernel_map_transpose_lowwidth_i32_runs;
        fprintf(stderr,
                "Kernel reduce_kernel_3382                     executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->reduce_kernel_3382_runs,
                (long) ctx->reduce_kernel_3382_total_runtime /
                (ctx->reduce_kernel_3382_runs !=
                 0 ? ctx->reduce_kernel_3382_runs : 1),
                (long) ctx->reduce_kernel_3382_total_runtime);
        ctx->total_runtime += ctx->reduce_kernel_3382_total_runtime;
        ctx->total_runs += ctx->reduce_kernel_3382_runs;
        if (ctx->debugging)
            fprintf(stderr, "Ran %d kernels with cumulative runtime: %6ldus\n",
                    ctx->total_runs, ctx->total_runtime);
    }
}
struct futrts_int32_t_int32_t {
    int32_t v0;
    int32_t v1;
} ;
struct futrts_ { } ;
static struct futrts_
futrts_map_transpose_opencl_i32(struct futhark_context *ctx,
                                struct memblock_device destmem_0,
                                int32_t destoffset_1,
                                struct memblock_device srcmem_2,
                                int32_t srcoffset_3, int32_t num_arrays_4,
                                int32_t x_elems_5, int32_t y_elems_6,
                                int32_t in_elems_7, int32_t out_elems_8);
static struct futrts_int32_t_int32_t futrts_main(struct futhark_context *ctx,
                                                 int64_t xs_mem_sizze_3423,
                                                 struct memblock_device xs_mem_3424,
                                                 int64_t ys_mem_sizze_3425,
                                                 struct memblock_device ys_mem_3426,
                                                 int32_t sizze_3261,
                                                 int32_t sizze_3262);
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
struct futrts_ futrts_map_transpose_opencl_i32(struct futhark_context *ctx,
                                               struct memblock_device destmem_0,
                                               int32_t destoffset_1,
                                               struct memblock_device srcmem_2,
                                               int32_t srcoffset_3,
                                               int32_t num_arrays_4,
                                               int32_t x_elems_5,
                                               int32_t y_elems_6,
                                               int32_t in_elems_7,
                                               int32_t out_elems_8)
{
    if (in_elems_7 == out_elems_8 && ((num_arrays_4 == 1 || x_elems_5 *
                                       y_elems_6 == in_elems_7) && (x_elems_5 ==
                                                                    1 ||
                                                                    y_elems_6 ==
                                                                    1))) {
        if (in_elems_7 * sizeof(int32_t) > 0) {
            OPENCL_SUCCEED(clEnqueueCopyBuffer(ctx->opencl.queue, srcmem_2.mem,
                                               destmem_0.mem, srcoffset_3,
                                               destoffset_1, in_elems_7 *
                                               sizeof(int32_t), 0, NULL, NULL));
            if (ctx->debugging)
                OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
        }
    } else {
        if (sle32(x_elems_5, squot32(16, 2)) && slt32(16, y_elems_6)) {
            int32_t muly_9 = squot32(16, x_elems_5);
            int32_t new_height_10;
            
            new_height_10 = squot32(y_elems_6 + muly_9 - 1, muly_9);
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          0, sizeof(destmem_0.mem),
                                          &destmem_0.mem));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          1, sizeof(destoffset_1),
                                          &destoffset_1));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          2, sizeof(srcmem_2.mem),
                                          &srcmem_2.mem));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          3, sizeof(srcoffset_3),
                                          &srcoffset_3));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          4, sizeof(x_elems_5), &x_elems_5));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          5, sizeof(y_elems_6), &y_elems_6));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          6, sizeof(in_elems_7), &in_elems_7));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          7, sizeof(out_elems_8),
                                          &out_elems_8));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          8, sizeof(muly_9), &muly_9));
            OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowwidth_i32,
                                          9, 272 * sizeof(int32_t), NULL));
            if (1 * (x_elems_5 + srem32(16 - srem32(x_elems_5, 16), 16)) *
                (new_height_10 + srem32(16 - srem32(new_height_10, 16), 16)) *
                num_arrays_4 != 0) {
                const size_t global_work_sizze_3491[3] = {x_elems_5 +
                                                          srem32(16 -
                                                                 srem32(x_elems_5,
                                                                        16),
                                                                 16),
                                                          new_height_10 +
                                                          srem32(16 -
                                                                 srem32(new_height_10,
                                                                        16),
                                                                 16),
                                                          num_arrays_4};
                const size_t local_work_sizze_3495[3] = {16, 16, 1};
                int64_t time_start_3492, time_end_3493;
                
                if (ctx->debugging) {
                    fprintf(stderr, "Launching %s with global work size [",
                            "fut_kernel_map_transpose_lowwidth_i32");
                    fprintf(stderr, "%zu", global_work_sizze_3491[0]);
                    fprintf(stderr, ", ");
                    fprintf(stderr, "%zu", global_work_sizze_3491[1]);
                    fprintf(stderr, ", ");
                    fprintf(stderr, "%zu", global_work_sizze_3491[2]);
                    fprintf(stderr, "].\n");
                    time_start_3492 = get_wall_time();
                }
                OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                      ctx->fut_kernel_map_transpose_lowwidth_i32,
                                                      3, NULL,
                                                      global_work_sizze_3491,
                                                      local_work_sizze_3495, 0,
                                                      NULL, NULL));
                if (ctx->debugging) {
                    OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
                    time_end_3493 = get_wall_time();
                    
                    long time_diff_3494 = time_end_3493 - time_start_3492;
                    
                    ctx->fut_kernel_map_transpose_lowwidth_i32_total_runtime +=
                        time_diff_3494;
                    ctx->fut_kernel_map_transpose_lowwidth_i32_runs++;
                    fprintf(stderr, "kernel %s runtime: %ldus\n",
                            "fut_kernel_map_transpose_lowwidth_i32",
                            (int) time_diff_3494);
                }
            }
        } else {
            if (sle32(y_elems_6, squot32(16, 2)) && slt32(16, x_elems_5)) {
                int32_t mulx_11 = squot32(16, y_elems_6);
                int32_t new_width_12;
                
                new_width_12 = squot32(x_elems_5 + mulx_11 - 1, mulx_11);
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              0, sizeof(destmem_0.mem),
                                              &destmem_0.mem));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              1, sizeof(destoffset_1),
                                              &destoffset_1));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              2, sizeof(srcmem_2.mem),
                                              &srcmem_2.mem));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              3, sizeof(srcoffset_3),
                                              &srcoffset_3));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              4, sizeof(x_elems_5),
                                              &x_elems_5));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              5, sizeof(y_elems_6),
                                              &y_elems_6));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              6, sizeof(in_elems_7),
                                              &in_elems_7));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              7, sizeof(out_elems_8),
                                              &out_elems_8));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              8, sizeof(mulx_11), &mulx_11));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_lowheight_i32,
                                              9, 272 * sizeof(int32_t), NULL));
                if (1 * (new_width_12 + srem32(16 - srem32(new_width_12, 16),
                                               16)) * (y_elems_6 + srem32(16 -
                                                                          srem32(y_elems_6,
                                                                                 16),
                                                                          16)) *
                    num_arrays_4 != 0) {
                    const size_t global_work_sizze_3496[3] = {new_width_12 +
                                                              srem32(16 -
                                                                     srem32(new_width_12,
                                                                            16),
                                                                     16),
                                                              y_elems_6 +
                                                              srem32(16 -
                                                                     srem32(y_elems_6,
                                                                            16),
                                                                     16),
                                                              num_arrays_4};
                    const size_t local_work_sizze_3500[3] = {16, 16, 1};
                    int64_t time_start_3497, time_end_3498;
                    
                    if (ctx->debugging) {
                        fprintf(stderr, "Launching %s with global work size [",
                                "fut_kernel_map_transpose_lowheight_i32");
                        fprintf(stderr, "%zu", global_work_sizze_3496[0]);
                        fprintf(stderr, ", ");
                        fprintf(stderr, "%zu", global_work_sizze_3496[1]);
                        fprintf(stderr, ", ");
                        fprintf(stderr, "%zu", global_work_sizze_3496[2]);
                        fprintf(stderr, "].\n");
                        time_start_3497 = get_wall_time();
                    }
                    OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                          ctx->fut_kernel_map_transpose_lowheight_i32,
                                                          3, NULL,
                                                          global_work_sizze_3496,
                                                          local_work_sizze_3500,
                                                          0, NULL, NULL));
                    if (ctx->debugging) {
                        OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
                        time_end_3498 = get_wall_time();
                        
                        long time_diff_3499 = time_end_3498 - time_start_3497;
                        
                        ctx->fut_kernel_map_transpose_lowheight_i32_total_runtime +=
                            time_diff_3499;
                        ctx->fut_kernel_map_transpose_lowheight_i32_runs++;
                        fprintf(stderr, "kernel %s runtime: %ldus\n",
                                "fut_kernel_map_transpose_lowheight_i32",
                                (int) time_diff_3499);
                    }
                }
            } else {
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              0, sizeof(destmem_0.mem),
                                              &destmem_0.mem));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              1, sizeof(destoffset_1),
                                              &destoffset_1));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              2, sizeof(srcmem_2.mem),
                                              &srcmem_2.mem));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              3, sizeof(srcoffset_3),
                                              &srcoffset_3));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              4, sizeof(x_elems_5),
                                              &x_elems_5));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              5, sizeof(y_elems_6),
                                              &y_elems_6));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              6, sizeof(in_elems_7),
                                              &in_elems_7));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              7, sizeof(out_elems_8),
                                              &out_elems_8));
                OPENCL_SUCCEED(clSetKernelArg(ctx->fut_kernel_map_transpose_i32,
                                              8, 272 * sizeof(int32_t), NULL));
                if (1 * (x_elems_5 + srem32(16 - srem32(x_elems_5, 16), 16)) *
                    (y_elems_6 + srem32(16 - srem32(y_elems_6, 16), 16)) *
                    num_arrays_4 != 0) {
                    const size_t global_work_sizze_3501[3] = {x_elems_5 +
                                                              srem32(16 -
                                                                     srem32(x_elems_5,
                                                                            16),
                                                                     16),
                                                              y_elems_6 +
                                                              srem32(16 -
                                                                     srem32(y_elems_6,
                                                                            16),
                                                                     16),
                                                              num_arrays_4};
                    const size_t local_work_sizze_3505[3] = {16, 16, 1};
                    int64_t time_start_3502, time_end_3503;
                    
                    if (ctx->debugging) {
                        fprintf(stderr, "Launching %s with global work size [",
                                "fut_kernel_map_transpose_i32");
                        fprintf(stderr, "%zu", global_work_sizze_3501[0]);
                        fprintf(stderr, ", ");
                        fprintf(stderr, "%zu", global_work_sizze_3501[1]);
                        fprintf(stderr, ", ");
                        fprintf(stderr, "%zu", global_work_sizze_3501[2]);
                        fprintf(stderr, "].\n");
                        time_start_3502 = get_wall_time();
                    }
                    OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                          ctx->fut_kernel_map_transpose_i32,
                                                          3, NULL,
                                                          global_work_sizze_3501,
                                                          local_work_sizze_3505,
                                                          0, NULL, NULL));
                    if (ctx->debugging) {
                        OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
                        time_end_3503 = get_wall_time();
                        
                        long time_diff_3504 = time_end_3503 - time_start_3502;
                        
                        ctx->fut_kernel_map_transpose_i32_total_runtime +=
                            time_diff_3504;
                        ctx->fut_kernel_map_transpose_i32_runs++;
                        fprintf(stderr, "kernel %s runtime: %ldus\n",
                                "fut_kernel_map_transpose_i32",
                                (int) time_diff_3504);
                    }
                }
            }
        }
    }
    
    struct futrts_ retval_3490;
    
    return retval_3490;
}
static struct futrts_int32_t_int32_t futrts_main(struct futhark_context *ctx,
                                                 int64_t xs_mem_sizze_3423,
                                                 struct memblock_device xs_mem_3424,
                                                 int64_t ys_mem_sizze_3425,
                                                 struct memblock_device ys_mem_3426,
                                                 int32_t sizze_3261,
                                                 int32_t sizze_3262)
{
    int32_t scalar_out_3475;
    int32_t scalar_out_3476;
    char assert_arg_3265 = sizze_3261 == sizze_3262;
    char shape_cert_3266;
    
    if (!assert_arg_3265) {
        fprintf(stderr, "Assertion failed at %s: %s\n",
                "process_idx.fut:10:5-10:5 -> process_idx.fut:11:9-11:9",
                "function arguments of wrong shape");
        exit(1);
    }
    
    int32_t group_sizze_3312;
    
    group_sizze_3312 = ctx->opencl.cfg.group_size;
    
    int32_t max_num_groups_3313;
    
    max_num_groups_3313 = ctx->opencl.cfg.num_groups;
    
    int32_t y_3314 = group_sizze_3312 - 1;
    int32_t x_3315 = sizze_3261 + y_3314;
    int32_t w_div_group_sizze_3316 = squot32(x_3315, group_sizze_3312);
    int32_t num_groups_maybe_zzero_3317 = smin32(w_div_group_sizze_3316,
                                                 max_num_groups_3313);
    int32_t num_groups_3318 = smax32(1, num_groups_maybe_zzero_3317);
    int32_t num_threads_3319 = num_groups_3318 * group_sizze_3312;
    int32_t y_3320 = num_threads_3319 - 1;
    int32_t x_3321 = sizze_3261 + y_3320;
    int32_t per_thread_elements_3322 = squot32(x_3321, num_threads_3319);
    int32_t y_3399 = smod32(sizze_3261, num_threads_3319);
    int32_t x_3400 = num_threads_3319 - y_3399;
    int32_t y_3401 = smod32(x_3400, num_threads_3319);
    int32_t padded_sizze_3402 = sizze_3261 + y_3401;
    int32_t per_chunk_3404 = squot32(padded_sizze_3402, num_threads_3319);
    int64_t binop_x_3428 = sext_i32_i64(y_3401);
    int64_t bytes_3427 = binop_x_3428 * 4;
    struct memblock_device mem_3429;
    
    mem_3429.references = NULL;
    memblock_alloc_device(ctx, &mem_3429, bytes_3427);
    
    int64_t binop_x_3431 = sext_i32_i64(padded_sizze_3402);
    int64_t bytes_3430 = binop_x_3431 * 4;
    struct memblock_device mem_3432;
    
    mem_3432.references = NULL;
    memblock_alloc_device(ctx, &mem_3432, bytes_3430);
    
    int32_t tmp_offs_3477 = 0;
    
    if (sizze_3261 * sizeof(int32_t) > 0) {
        OPENCL_SUCCEED(clEnqueueCopyBuffer(ctx->opencl.queue, xs_mem_3424.mem,
                                           mem_3432.mem, 0, tmp_offs_3477 * 4,
                                           sizze_3261 * sizeof(int32_t), 0,
                                           NULL, NULL));
        if (ctx->debugging)
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
    }
    tmp_offs_3477 += sizze_3261;
    if (y_3401 * sizeof(int32_t) > 0) {
        OPENCL_SUCCEED(clEnqueueCopyBuffer(ctx->opencl.queue, mem_3429.mem,
                                           mem_3432.mem, 0, tmp_offs_3477 * 4,
                                           y_3401 * sizeof(int32_t), 0, NULL,
                                           NULL));
        if (ctx->debugging)
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
    }
    tmp_offs_3477 += y_3401;
    
    int64_t binop_y_3434 = sext_i32_i64(per_chunk_3404);
    int64_t binop_x_3435 = 4 * binop_y_3434;
    int64_t binop_y_3436 = sext_i32_i64(num_threads_3319);
    int64_t bytes_3433 = binop_x_3435 * binop_y_3436;
    struct memblock_device mem_3437;
    
    mem_3437.references = NULL;
    memblock_alloc_device(ctx, &mem_3437, bytes_3433);
    
    struct futrts_ call_ret_3507;
    
    call_ret_3507 = futrts_map_transpose_opencl_i32(ctx, mem_3437, 0, mem_3432,
                                                    0, 1, per_chunk_3404,
                                                    num_threads_3319,
                                                    num_threads_3319 *
                                                    per_chunk_3404,
                                                    num_threads_3319 *
                                                    per_chunk_3404);
    
    int32_t y_3411 = smod32(sizze_3262, num_threads_3319);
    int32_t x_3412 = num_threads_3319 - y_3411;
    int32_t y_3413 = smod32(x_3412, num_threads_3319);
    int32_t padded_sizze_3414 = sizze_3262 + y_3413;
    int32_t per_chunk_3416 = squot32(padded_sizze_3414, num_threads_3319);
    int64_t binop_x_3439 = sext_i32_i64(y_3413);
    int64_t bytes_3438 = binop_x_3439 * 4;
    struct memblock_device mem_3440;
    
    mem_3440.references = NULL;
    memblock_alloc_device(ctx, &mem_3440, bytes_3438);
    
    int64_t binop_x_3442 = sext_i32_i64(padded_sizze_3414);
    int64_t bytes_3441 = binop_x_3442 * 4;
    struct memblock_device mem_3443;
    
    mem_3443.references = NULL;
    memblock_alloc_device(ctx, &mem_3443, bytes_3441);
    
    int32_t tmp_offs_3478 = 0;
    
    if (sizze_3262 * sizeof(int32_t) > 0) {
        OPENCL_SUCCEED(clEnqueueCopyBuffer(ctx->opencl.queue, ys_mem_3426.mem,
                                           mem_3443.mem, 0, tmp_offs_3478 * 4,
                                           sizze_3262 * sizeof(int32_t), 0,
                                           NULL, NULL));
        if (ctx->debugging)
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
    }
    tmp_offs_3478 += sizze_3262;
    if (y_3413 * sizeof(int32_t) > 0) {
        OPENCL_SUCCEED(clEnqueueCopyBuffer(ctx->opencl.queue, mem_3440.mem,
                                           mem_3443.mem, 0, tmp_offs_3478 * 4,
                                           y_3413 * sizeof(int32_t), 0, NULL,
                                           NULL));
        if (ctx->debugging)
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
    }
    tmp_offs_3478 += y_3413;
    
    int64_t binop_y_3445 = sext_i32_i64(per_chunk_3416);
    int64_t binop_x_3446 = 4 * binop_y_3445;
    int64_t bytes_3444 = binop_x_3446 * binop_y_3436;
    struct memblock_device mem_3448;
    
    mem_3448.references = NULL;
    memblock_alloc_device(ctx, &mem_3448, bytes_3444);
    
    struct futrts_ call_ret_3508;
    
    call_ret_3508 = futrts_map_transpose_opencl_i32(ctx, mem_3448, 0, mem_3443,
                                                    0, 1, per_chunk_3416,
                                                    num_threads_3319,
                                                    num_threads_3319 *
                                                    per_chunk_3416,
                                                    num_threads_3319 *
                                                    per_chunk_3416);
    
    int64_t binop_x_3456 = sext_i32_i64(num_groups_3318);
    int64_t bytes_3455 = binop_x_3456 * 4;
    struct memblock_device mem_3457;
    
    mem_3457.references = NULL;
    memblock_alloc_device(ctx, &mem_3457, bytes_3455);
    
    struct memblock_device mem_3460;
    
    mem_3460.references = NULL;
    memblock_alloc_device(ctx, &mem_3460, bytes_3455);
    
    int64_t binop_y_3450 = sext_i32_i64(group_sizze_3312);
    int64_t bytes_3449 = 4 * binop_y_3450;
    struct memblock_local mem_3451;
    
    mem_3451.references = NULL;
    
    struct memblock_local mem_3454;
    
    mem_3454.references = NULL;
    if (ctx->debugging)
        fprintf(stderr, "%s: %d\n", "input size", (int) sizze_3261);
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 0,
                                  bytes_3449, NULL));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 1,
                                  bytes_3449, NULL));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 2,
                                  sizeof(sizze_3261), &sizze_3261));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 3,
                                  sizeof(num_threads_3319), &num_threads_3319));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 4,
                                  sizeof(per_thread_elements_3322),
                                  &per_thread_elements_3322));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 5,
                                  sizeof(per_chunk_3404), &per_chunk_3404));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 6,
                                  sizeof(per_chunk_3416), &per_chunk_3416));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 7,
                                  sizeof(mem_3437.mem), &mem_3437.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 8,
                                  sizeof(mem_3448.mem), &mem_3448.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 9,
                                  sizeof(mem_3457.mem), &mem_3457.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->chunked_reduce_kernel_3328, 10,
                                  sizeof(mem_3460.mem), &mem_3460.mem));
    if (1 * (num_groups_3318 * group_sizze_3312) != 0) {
        const size_t global_work_sizze_3509[1] = {num_groups_3318 *
                     group_sizze_3312};
        const size_t local_work_sizze_3513[1] = {group_sizze_3312};
        int64_t time_start_3510, time_end_3511;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "chunked_reduce_kernel_3328");
            fprintf(stderr, "%zu", global_work_sizze_3509[0]);
            fprintf(stderr, "].\n");
            time_start_3510 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                              ctx->chunked_reduce_kernel_3328,
                                              1, NULL, global_work_sizze_3509,
                                              local_work_sizze_3513, 0, NULL,
                                              NULL));
        if (ctx->debugging) {
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
            time_end_3511 = get_wall_time();
            
            long time_diff_3512 = time_end_3511 - time_start_3510;
            
            ctx->chunked_reduce_kernel_3328_total_runtime += time_diff_3512;
            ctx->chunked_reduce_kernel_3328_runs++;
            fprintf(stderr, "kernel %s runtime: %ldus\n",
                    "chunked_reduce_kernel_3328", (int) time_diff_3512);
        }
    }
    
    struct memblock_device mem_3469;
    
    mem_3469.references = NULL;
    memblock_alloc_device(ctx, &mem_3469, 4);
    
    struct memblock_device mem_3472;
    
    mem_3472.references = NULL;
    memblock_alloc_device(ctx, &mem_3472, 4);
    
    struct memblock_local mem_3463;
    
    mem_3463.references = NULL;
    
    struct memblock_local mem_3466;
    
    mem_3466.references = NULL;
    OPENCL_SUCCEED(clSetKernelArg(ctx->reduce_kernel_3382, 0, bytes_3449,
                                  NULL));
    OPENCL_SUCCEED(clSetKernelArg(ctx->reduce_kernel_3382, 1, bytes_3449,
                                  NULL));
    OPENCL_SUCCEED(clSetKernelArg(ctx->reduce_kernel_3382, 2,
                                  sizeof(num_groups_3318), &num_groups_3318));
    OPENCL_SUCCEED(clSetKernelArg(ctx->reduce_kernel_3382, 3,
                                  sizeof(mem_3457.mem), &mem_3457.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->reduce_kernel_3382, 4,
                                  sizeof(mem_3460.mem), &mem_3460.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->reduce_kernel_3382, 5,
                                  sizeof(mem_3469.mem), &mem_3469.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->reduce_kernel_3382, 6,
                                  sizeof(mem_3472.mem), &mem_3472.mem));
    if (1 * group_sizze_3312 != 0) {
        const size_t global_work_sizze_3514[1] = {group_sizze_3312};
        const size_t local_work_sizze_3518[1] = {group_sizze_3312};
        int64_t time_start_3515, time_end_3516;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "reduce_kernel_3382");
            fprintf(stderr, "%zu", global_work_sizze_3514[0]);
            fprintf(stderr, "].\n");
            time_start_3515 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                              ctx->reduce_kernel_3382, 1, NULL,
                                              global_work_sizze_3514,
                                              local_work_sizze_3518, 0, NULL,
                                              NULL));
        if (ctx->debugging) {
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
            time_end_3516 = get_wall_time();
            
            long time_diff_3517 = time_end_3516 - time_start_3515;
            
            ctx->reduce_kernel_3382_total_runtime += time_diff_3517;
            ctx->reduce_kernel_3382_runs++;
            fprintf(stderr, "kernel %s runtime: %ldus\n", "reduce_kernel_3382",
                    (int) time_diff_3517);
        }
    }
    
    int32_t read_res_3519;
    
    OPENCL_SUCCEED(clEnqueueReadBuffer(ctx->opencl.queue, mem_3469.mem, CL_TRUE,
                                       0, sizeof(int32_t), &read_res_3519, 0,
                                       NULL, NULL));
    
    int32_t res_3277 = read_res_3519;
    int32_t read_res_3520;
    
    OPENCL_SUCCEED(clEnqueueReadBuffer(ctx->opencl.queue, mem_3472.mem, CL_TRUE,
                                       0, sizeof(int32_t), &read_res_3520, 0,
                                       NULL, NULL));
    
    int32_t res_3278 = read_res_3520;
    
    scalar_out_3475 = res_3277;
    scalar_out_3476 = res_3278;
    
    struct futrts_int32_t_int32_t retval_3506;
    
    retval_3506.v0 = scalar_out_3475;
    retval_3506.v1 = scalar_out_3476;
    memblock_unref_device(ctx, &mem_3429);
    memblock_unref_device(ctx, &mem_3432);
    memblock_unref_device(ctx, &mem_3437);
    memblock_unref_device(ctx, &mem_3440);
    memblock_unref_device(ctx, &mem_3443);
    memblock_unref_device(ctx, &mem_3448);
    memblock_unref_device(ctx, &mem_3457);
    memblock_unref_device(ctx, &mem_3460);
    memblock_unref_local(ctx, &mem_3451);
    memblock_unref_local(ctx, &mem_3454);
    memblock_unref_device(ctx, &mem_3469);
    memblock_unref_device(ctx, &mem_3472);
    memblock_unref_local(ctx, &mem_3463);
    memblock_unref_local(ctx, &mem_3466);
    return retval_3506;
}
struct futhark_i32_1d {
    struct memblock_device mem;
    int64_t shape[1];
} ;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                          int32_t *data, int dim0)
{
    struct futhark_i32_1d *arr = malloc(sizeof(struct futhark_i32_1d));
    
    if (arr == NULL)
        return NULL;
    arr->mem.references = NULL;
    memblock_alloc_device(ctx, &arr->mem, dim0 * sizeof(int32_t));
    if (dim0 * sizeof(int32_t) > 0)
        OPENCL_SUCCEED(clEnqueueWriteBuffer(ctx->opencl.queue, arr->mem.mem,
                                            CL_TRUE, 0, dim0 * sizeof(int32_t),
                                            data + 0, 0, NULL, NULL));
    arr->shape[0] = dim0;
    return arr;
}
int futhark_free_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr)
{
    memblock_unref_device(ctx, &arr->mem);
    free(arr);
    return 0;
}
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data)
{
    if (arr->shape[0] * sizeof(int32_t) > 0)
        OPENCL_SUCCEED(clEnqueueReadBuffer(ctx->opencl.queue, arr->mem.mem,
                                           CL_TRUE, 0, arr->shape[0] *
                                           sizeof(int32_t), data + 0, 0, NULL,
                                           NULL));
    return 0;
}
int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                              struct futhark_i32_1d *arr)
{
    return arr->shape;
}
int futhark_main(struct futhark_context *ctx, int32_t *out0, int32_t *out1,
                 struct futhark_i32_1d *in0, struct futhark_i32_1d *in1)
{
    int64_t xs_mem_sizze_3423;
    struct memblock_device xs_mem_3424;
    
    xs_mem_3424.references = NULL;
    
    int64_t ys_mem_sizze_3425;
    struct memblock_device ys_mem_3426;
    
    ys_mem_3426.references = NULL;
    
    int32_t sizze_3261;
    int32_t sizze_3262;
    int32_t scalar_out_3475;
    int32_t scalar_out_3476;
    
    xs_mem_3424 = in0->mem;
    xs_mem_sizze_3423 = in0->mem.size;
    sizze_3261 = in0->shape[0];
    ys_mem_3426 = in1->mem;
    ys_mem_sizze_3425 = in1->mem.size;
    sizze_3262 = in1->shape[0];
    
    struct futrts_int32_t_int32_t ret_3521;
    
    ret_3521 = futrts_main(ctx, xs_mem_sizze_3423, xs_mem_3424,
                           ys_mem_sizze_3425, ys_mem_3426, sizze_3261,
                           sizze_3262);
    scalar_out_3475 = ret_3521.v0;
    scalar_out_3476 = ret_3521.v1;
    *out0 = scalar_out_3475;
    *out1 = scalar_out_3476;
    return 0;
}
