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

struct futhark_f64_1d *futhark_new_f64_1d(struct futhark_context *ctx,
                                          double *data, int dim0);
int futhark_free_f64_1d(struct futhark_context *ctx,
                        struct futhark_f64_1d *arr);
int futhark_values_f64_1d(struct futhark_context *ctx,
                          struct futhark_f64_1d *arr, double *data);
int64_t *futhark_shape_f64_1d(struct futhark_context *ctx,
                              struct futhark_f64_1d *arr);
struct futhark_f64_1d ;

/*
 * Opaque values
*/


/*
 * Entry points
*/

int futhark_main(struct futhark_context *ctx, double *out0,
                 struct futhark_f64_1d *in0, struct futhark_f64_1d *in1);

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
    struct futhark_f64_1d *read_value_3450;
    int64_t read_shape_3451[1];
    double *read_arr_3452 = NULL;
    
    errno = 0;
    if (read_array(&f64, (void **) &read_arr_3452, read_shape_3451, 1) != 0)
        panic(1, "Failed reading input of type %s%s (errno: %s).\n", "[]",
              f64.type_name, strerror(errno));
    
    struct futhark_f64_1d *read_value_3453;
    int64_t read_shape_3454[1];
    double *read_arr_3455 = NULL;
    
    errno = 0;
    if (read_array(&f64, (void **) &read_arr_3455, read_shape_3454, 1) != 0)
        panic(1, "Failed reading input of type %s%s (errno: %s).\n", "[]",
              f64.type_name, strerror(errno));
    
    double result_3456;
    
    if (perform_warmup) {
        time_runs = 0;
        assert((read_value_3450 = futhark_new_f64_1d(ctx, read_arr_3452,
                                                     read_shape_3451[0])) != 0);
        assert((read_value_3453 = futhark_new_f64_1d(ctx, read_arr_3455,
                                                     read_shape_3454[0])) != 0);
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_3456, read_value_3450,
                            read_value_3453) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        assert(futhark_free_f64_1d(ctx, read_value_3450) == 0);
        assert(futhark_free_f64_1d(ctx, read_value_3453) == 0);
        ;
    }
    time_runs = 1;
    /* Proper run. */
    for (int run = 0; run < num_runs; run++) {
        assert((read_value_3450 = futhark_new_f64_1d(ctx, read_arr_3452,
                                                     read_shape_3451[0])) != 0);
        assert((read_value_3453 = futhark_new_f64_1d(ctx, read_arr_3455,
                                                     read_shape_3454[0])) != 0);
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_3456, read_value_3450,
                            read_value_3453) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        assert(futhark_free_f64_1d(ctx, read_value_3450) == 0);
        assert(futhark_free_f64_1d(ctx, read_value_3453) == 0);
        if (run < num_runs - 1) {
            ;
        }
    }
    free(read_arr_3452);
    free(read_arr_3455);
    write_scalar(stdout, binary_output, &f64, &result_3456);
    printf("\n");
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
           {"#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n__kernel void dummy_kernel(__global unsigned char *dummy, int n)\n{\n    const int thread_gid = get_global_id(0);\n    \n    if (thread_gid >= n)\n        return;\n}\ntypedef char int8_t;\ntypedef short int16_t;\ntypedef int int32_t;\ntypedef long int64_t;\ntypedef uchar uint8_t;\ntypedef ushort uint16_t;\ntypedef uint uint32_t;\ntypedef ulong uint64_t;\n#define ALIGNED_LOCAL_MEMORY(m,size) __local unsigned char m[size] __attribute__ ((align))\nstatic inline int8_t add8(int8_t x, int8_t y)\n{\n    return x + y;\n}\nstatic inline int16_t add16(int16_t x, int16_t y)\n{\n    return x + y;\n}\nstatic inline int32_t add32(int32_t x, int32_t y)\n{\n    return x + y;\n}\nstatic inline int64_t add64(int64_t x, int64_t y)\n{\n    return x + y;\n}\nstatic inline int8_t sub8(int8_t x, int8_t y)\n{\n    return x - y;\n}\nstatic inline int16_t sub16(int16_t x, int16_t y)\n{\n    return x - y;\n}\nstatic inline int32_t sub32(int32_t x, int32_t y)\n{\n    return x - y;\n}\nstatic inline int64_t sub64(int64_t x, int64_t y)\n{\n    return x - y;\n}\nstatic inline int8_t mul8(int8_t x, int8_t y)\n{\n    return x * y;\n}\nstatic inline int16_t mul16(int16_t x, int16_t y)\n{\n    return x * y;\n}\nstatic inline int32_t mul32(int32_t x, int32_t y)\n{\n    return x * y;\n}\nstatic inline int64_t mul64(int64_t x, int64_t y)\n{\n    return x * y;\n}\nstatic inline uint8_t udiv8(uint8_t x, uint8_t y)\n{\n    return x / y;\n}\nstatic inline uint16_t udiv16(uint16_t x, uint16_t y)\n{\n    return x / y;\n}\nstatic inline uint32_t udiv32(uint32_t x, uint32_t y)\n{\n    return x / y;\n}\nstatic inline uint64_t udiv64(uint64_t x, uint64_t y)\n{\n    return x / y;\n}\nstatic inline uint8_t umod8(uint8_t x, uint8_t y)\n{\n    return x % y;\n}\nstatic inline uint16_t umod16(uint16_t x, uint16_t y)\n{\n    return x % y;\n}\nstatic inline uint32_t umod32(uint32_t x, uint32_t y)\n{\n    return x % y;\n}\nstatic inline uint64_t umod64(uint64_t x, uint64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t sdiv8(int8_t x, int8_t y)\n{\n    int8_t q = x / y",
            ";\n    int8_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int16_t sdiv16(int16_t x, int16_t y)\n{\n    int16_t q = x / y;\n    int16_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int32_t sdiv32(int32_t x, int32_t y)\n{\n    int32_t q = x / y;\n    int32_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int64_t sdiv64(int64_t x, int64_t y)\n{\n    int64_t q = x / y;\n    int64_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int8_t smod8(int8_t x, int8_t y)\n{\n    int8_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int16_t smod16(int16_t x, int16_t y)\n{\n    int16_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int32_t smod32(int32_t x, int32_t y)\n{\n    int32_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int64_t smod64(int64_t x, int64_t y)\n{\n    int64_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int8_t squot8(int8_t x, int8_t y)\n{\n    return x / y;\n}\nstatic inline int16_t squot16(int16_t x, int16_t y)\n{\n    return x / y;\n}\nstatic inline int32_t squot32(int32_t x, int32_t y)\n{\n    return x / y;\n}\nstatic inline int64_t squot64(int64_t x, int64_t y)\n{\n    return x / y;\n}\nstatic inline int8_t srem8(int8_t x, int8_t y)\n{\n    return x % y;\n}\nstatic inline int16_t srem16(int16_t x, int16_t y)\n{\n    return x % y;\n}\nstatic inline int32_t srem32(int32_t x, int32_t y)\n{\n    return x % y;\n}\nstatic inline int64_t srem64(int64_t x, int64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t smin8(int8_t x, int8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int16_t smin16(int16_t x, int16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int32_t smin32(int32_t x, int32_t y)\n{\n    return x < y ? x : y;\n}\ns",
            "tatic inline int64_t smin64(int64_t x, int64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint8_t umin8(uint8_t x, uint8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint16_t umin16(uint16_t x, uint16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint32_t umin32(uint32_t x, uint32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint64_t umin64(uint64_t x, uint64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int8_t smax8(int8_t x, int8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int16_t smax16(int16_t x, int16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int32_t smax32(int32_t x, int32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int64_t smax64(int64_t x, int64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t umax8(uint8_t x, uint8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint16_t umax16(uint16_t x, uint16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint32_t umax32(uint32_t x, uint32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint64_t umax64(uint64_t x, uint64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t shl8(uint8_t x, uint8_t y)\n{\n    return x << y;\n}\nstatic inline uint16_t shl16(uint16_t x, uint16_t y)\n{\n    return x << y;\n}\nstatic inline uint32_t shl32(uint32_t x, uint32_t y)\n{\n    return x << y;\n}\nstatic inline uint64_t shl64(uint64_t x, uint64_t y)\n{\n    return x << y;\n}\nstatic inline uint8_t lshr8(uint8_t x, uint8_t y)\n{\n    return x >> y;\n}\nstatic inline uint16_t lshr16(uint16_t x, uint16_t y)\n{\n    return x >> y;\n}\nstatic inline uint32_t lshr32(uint32_t x, uint32_t y)\n{\n    return x >> y;\n}\nstatic inline uint64_t lshr64(uint64_t x, uint64_t y)\n{\n    return x >> y;\n}\nstatic inline int8_t ashr8(int8_t x, int8_t y)\n{\n    return x >> y;\n}\nstatic inline int16_t ashr16(int16_t x, int16_t y)\n{\n    return x >> y;\n}\nstatic inline int32_t ashr32(int32_t x, int32_t y)\n{\n    return x >> y;\n}\nstatic inline int64_t ashr64(int64_t x, int64_t y)\n{\n    return x >> y;\n}\nstatic inline uint8_t and8(uint8_t x, ui",
            "nt8_t y)\n{\n    return x & y;\n}\nstatic inline uint16_t and16(uint16_t x, uint16_t y)\n{\n    return x & y;\n}\nstatic inline uint32_t and32(uint32_t x, uint32_t y)\n{\n    return x & y;\n}\nstatic inline uint64_t and64(uint64_t x, uint64_t y)\n{\n    return x & y;\n}\nstatic inline uint8_t or8(uint8_t x, uint8_t y)\n{\n    return x | y;\n}\nstatic inline uint16_t or16(uint16_t x, uint16_t y)\n{\n    return x | y;\n}\nstatic inline uint32_t or32(uint32_t x, uint32_t y)\n{\n    return x | y;\n}\nstatic inline uint64_t or64(uint64_t x, uint64_t y)\n{\n    return x | y;\n}\nstatic inline uint8_t xor8(uint8_t x, uint8_t y)\n{\n    return x ^ y;\n}\nstatic inline uint16_t xor16(uint16_t x, uint16_t y)\n{\n    return x ^ y;\n}\nstatic inline uint32_t xor32(uint32_t x, uint32_t y)\n{\n    return x ^ y;\n}\nstatic inline uint64_t xor64(uint64_t x, uint64_t y)\n{\n    return x ^ y;\n}\nstatic inline char ult8(uint8_t x, uint8_t y)\n{\n    return x < y;\n}\nstatic inline char ult16(uint16_t x, uint16_t y)\n{\n    return x < y;\n}\nstatic inline char ult32(uint32_t x, uint32_t y)\n{\n    return x < y;\n}\nstatic inline char ult64(uint64_t x, uint64_t y)\n{\n    return x < y;\n}\nstatic inline char ule8(uint8_t x, uint8_t y)\n{\n    return x <= y;\n}\nstatic inline char ule16(uint16_t x, uint16_t y)\n{\n    return x <= y;\n}\nstatic inline char ule32(uint32_t x, uint32_t y)\n{\n    return x <= y;\n}\nstatic inline char ule64(uint64_t x, uint64_t y)\n{\n    return x <= y;\n}\nstatic inline char slt8(int8_t x, int8_t y)\n{\n    return x < y;\n}\nstatic inline char slt16(int16_t x, int16_t y)\n{\n    return x < y;\n}\nstatic inline char slt32(int32_t x, int32_t y)\n{\n    return x < y;\n}\nstatic inline char slt64(int64_t x, int64_t y)\n{\n    return x < y;\n}\nstatic inline char sle8(int8_t x, int8_t y)\n{\n    return x <= y;\n}\nstatic inline char sle16(int16_t x, int16_t y)\n{\n    return x <= y;\n}\nstatic inline char sle32(int32_t x, int32_t y)\n{\n    return x <= y;\n}\nstatic inline char sle64(int64_t x, int64_t y)\n{\n    return x <= y;\n}\nstatic inline int8_t pow8(int8_t x, int8",
            "_t y)\n{\n    int8_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int16_t pow16(int16_t x, int16_t y)\n{\n    int16_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int32_t pow32(int32_t x, int32_t y)\n{\n    int32_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int64_t pow64(int64_t x, int64_t y)\n{\n    int64_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int8_t sext_i8_i8(int8_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i8_i16(int8_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i8_i32(int8_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i8_i64(int8_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i16_i8(int16_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i16_i16(int16_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i16_i32(int16_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i16_i64(int16_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i32_i8(int32_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i32_i16(int32_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i32_i32(int32_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i32_i64(int32_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i64_i8(int64_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i64_i16(int64_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i64_i32(int64_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i64_i64(int64_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i8_i8(uint8_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i8_i16(uint8_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i8_i32(uint8_t x)\n{\n    return x;\n}",
            "\nstatic inline uint64_t zext_i8_i64(uint8_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i16_i8(uint16_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i16_i16(uint16_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i16_i32(uint16_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i16_i64(uint16_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i32_i8(uint32_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i32_i16(uint32_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i32_i32(uint32_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i32_i64(uint32_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i64_i8(uint64_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i64_i16(uint64_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i64_i32(uint64_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i64_i64(uint64_t x)\n{\n    return x;\n}\nstatic inline float fdiv32(float x, float y)\n{\n    return x / y;\n}\nstatic inline float fadd32(float x, float y)\n{\n    return x + y;\n}\nstatic inline float fsub32(float x, float y)\n{\n    return x - y;\n}\nstatic inline float fmul32(float x, float y)\n{\n    return x * y;\n}\nstatic inline float fmin32(float x, float y)\n{\n    return x < y ? x : y;\n}\nstatic inline float fmax32(float x, float y)\n{\n    return x < y ? y : x;\n}\nstatic inline float fpow32(float x, float y)\n{\n    return pow(x, y);\n}\nstatic inline char cmplt32(float x, float y)\n{\n    return x < y;\n}\nstatic inline char cmple32(float x, float y)\n{\n    return x <= y;\n}\nstatic inline float sitofp_i8_f32(int8_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i16_f32(int16_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i32_f32(int32_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i64_f32(int64_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i8_f32(uint8_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i16_f32(uint16_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i32_f32(uint32_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i64_f32(uint64_t x)\n{\n    return x;\n}\nstatic inline",
            " int8_t fptosi_f32_i8(float x)\n{\n    return x;\n}\nstatic inline int16_t fptosi_f32_i16(float x)\n{\n    return x;\n}\nstatic inline int32_t fptosi_f32_i32(float x)\n{\n    return x;\n}\nstatic inline int64_t fptosi_f32_i64(float x)\n{\n    return x;\n}\nstatic inline uint8_t fptoui_f32_i8(float x)\n{\n    return x;\n}\nstatic inline uint16_t fptoui_f32_i16(float x)\n{\n    return x;\n}\nstatic inline uint32_t fptoui_f32_i32(float x)\n{\n    return x;\n}\nstatic inline uint64_t fptoui_f32_i64(float x)\n{\n    return x;\n}\nstatic inline float futrts_log32(float x)\n{\n    return log(x);\n}\nstatic inline float futrts_sqrt32(float x)\n{\n    return sqrt(x);\n}\nstatic inline float futrts_exp32(float x)\n{\n    return exp(x);\n}\nstatic inline float futrts_cos32(float x)\n{\n    return cos(x);\n}\nstatic inline float futrts_sin32(float x)\n{\n    return sin(x);\n}\nstatic inline float futrts_acos32(float x)\n{\n    return acos(x);\n}\nstatic inline float futrts_asin32(float x)\n{\n    return asin(x);\n}\nstatic inline float futrts_atan32(float x)\n{\n    return atan(x);\n}\nstatic inline float futrts_atan2_32(float x, float y)\n{\n    return atan2(x, y);\n}\nstatic inline char futrts_isnan32(float x)\n{\n    return isnan(x);\n}\nstatic inline char futrts_isinf32(float x)\n{\n    return isinf(x);\n}\nstatic inline int32_t futrts_to_bits32(float x)\n{\n    union {\n        float f;\n        int32_t t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\nstatic inline float futrts_from_bits32(int32_t x)\n{\n    union {\n        int32_t f;\n        float t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\nstatic inline double fdiv64(double x, double y)\n{\n    return x / y;\n}\nstatic inline double fadd64(double x, double y)\n{\n    return x + y;\n}\nstatic inline double fsub64(double x, double y)\n{\n    return x - y;\n}\nstatic inline double fmul64(double x, double y)\n{\n    return x * y;\n}\nstatic inline double fmin64(double x, double y)\n{\n    return x < y ? x : y;\n}\nstatic inline double fmax64(double x, double y)\n{\n    return x < y ? y : x;\n}\nstatic inline double fpow64(dou",
            "ble x, double y)\n{\n    return pow(x, y);\n}\nstatic inline char cmplt64(double x, double y)\n{\n    return x < y;\n}\nstatic inline char cmple64(double x, double y)\n{\n    return x <= y;\n}\nstatic inline double sitofp_i8_f64(int8_t x)\n{\n    return x;\n}\nstatic inline double sitofp_i16_f64(int16_t x)\n{\n    return x;\n}\nstatic inline double sitofp_i32_f64(int32_t x)\n{\n    return x;\n}\nstatic inline double sitofp_i64_f64(int64_t x)\n{\n    return x;\n}\nstatic inline double uitofp_i8_f64(uint8_t x)\n{\n    return x;\n}\nstatic inline double uitofp_i16_f64(uint16_t x)\n{\n    return x;\n}\nstatic inline double uitofp_i32_f64(uint32_t x)\n{\n    return x;\n}\nstatic inline double uitofp_i64_f64(uint64_t x)\n{\n    return x;\n}\nstatic inline int8_t fptosi_f64_i8(double x)\n{\n    return x;\n}\nstatic inline int16_t fptosi_f64_i16(double x)\n{\n    return x;\n}\nstatic inline int32_t fptosi_f64_i32(double x)\n{\n    return x;\n}\nstatic inline int64_t fptosi_f64_i64(double x)\n{\n    return x;\n}\nstatic inline uint8_t fptoui_f64_i8(double x)\n{\n    return x;\n}\nstatic inline uint16_t fptoui_f64_i16(double x)\n{\n    return x;\n}\nstatic inline uint32_t fptoui_f64_i32(double x)\n{\n    return x;\n}\nstatic inline uint64_t fptoui_f64_i64(double x)\n{\n    return x;\n}\nstatic inline double futrts_log64(double x)\n{\n    return log(x);\n}\nstatic inline double futrts_sqrt64(double x)\n{\n    return sqrt(x);\n}\nstatic inline double futrts_exp64(double x)\n{\n    return exp(x);\n}\nstatic inline double futrts_cos64(double x)\n{\n    return cos(x);\n}\nstatic inline double futrts_sin64(double x)\n{\n    return sin(x);\n}\nstatic inline double futrts_acos64(double x)\n{\n    return acos(x);\n}\nstatic inline double futrts_asin64(double x)\n{\n    return asin(x);\n}\nstatic inline double futrts_atan64(double x)\n{\n    return atan(x);\n}\nstatic inline double futrts_atan2_64(double x, double y)\n{\n    return atan2(x, y);\n}\nstatic inline char futrts_isnan64(double x)\n{\n    return isnan(x);\n}\nstatic inline char futrts_isinf64(double x)\n{\n    return isinf(x);\n}\nstatic inli",
            "ne int64_t futrts_to_bits64(double x)\n{\n    union {\n        double f;\n        int64_t t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\nstatic inline double futrts_from_bits64(int64_t x)\n{\n    union {\n        int64_t f;\n        double t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\nstatic inline float fpconv_f32_f32(float x)\n{\n    return x;\n}\nstatic inline double fpconv_f32_f64(float x)\n{\n    return x;\n}\nstatic inline float fpconv_f64_f32(double x)\n{\n    return x;\n}\nstatic inline double fpconv_f64_f64(double x)\n{\n    return x;\n}\n#define group_sizze_3280 (DEFAULT_GROUP_SIZE)\n#define y_3282 (DEFAULT_GROUP_SIZE - 1)\n__kernel void map_kernel_3374(int32_t sizze_3233, int32_t y_3323, __global\n                              unsigned char *mem_3383, __global\n                              unsigned char *mem_3398, __global\n                              unsigned char *mem_3401)\n{\n    int32_t wave_sizze_3428;\n    int32_t group_sizze_3429;\n    char thread_active_3430;\n    int32_t j_3359;\n    int32_t global_tid_3374;\n    int32_t local_tid_3375;\n    int32_t group_id_3376;\n    \n    global_tid_3374 = get_global_id(0);\n    local_tid_3375 = get_local_id(0);\n    group_sizze_3429 = get_local_size(0);\n    wave_sizze_3428 = LOCKSTEP_WIDTH;\n    group_id_3376 = get_group_id(0);\n    j_3359 = global_tid_3374;\n    thread_active_3430 = slt32(j_3359, sizze_3233);\n    \n    int32_t y_3357;\n    int32_t group_id_3364;\n    char cond_3365;\n    int32_t final_result_3367;\n    \n    if (thread_active_3430) {\n        y_3357 = *(__global int32_t *) &mem_3383[j_3359 * 4];\n        group_id_3364 = squot32(j_3359, y_3323);\n        cond_3365 = 0 == group_id_3364;\n        if (cond_3365) {\n            final_result_3367 = y_3357;\n        } else {\n            int32_t carry_in_index_3366 = group_id_3364 - 1;\n            int32_t x_3356 = *(__global\n                               int32_t *) &mem_3398[carry_in_index_3366 * 4];\n            int32_t zz_3358 = x_3356 + y_3357;\n            \n            final_result_3367 = zz_33",
            "58;\n        }\n    }\n    if (thread_active_3430) {\n        *(__global int32_t *) &mem_3401[j_3359 * 4] = final_result_3367;\n    }\n}\n__kernel void scan1_kernel_3315(__local volatile int64_t *mem_aligned_0,\n                                int32_t sizze_3233, int32_t num_iterations_3320,\n                                int32_t y_3323, __global\n                                unsigned char *xs_mem_3378, __global\n                                unsigned char *ys_mem_3380, __global\n                                unsigned char *mem_3383, __global\n                                unsigned char *mem_3392)\n{\n    __local volatile char *restrict mem_3386 = mem_aligned_0;\n    int32_t wave_sizze_3406;\n    int32_t group_sizze_3407;\n    char thread_active_3408;\n    int32_t global_tid_3315;\n    int32_t local_tid_3316;\n    int32_t group_id_3317;\n    \n    global_tid_3315 = get_global_id(0);\n    local_tid_3316 = get_local_id(0);\n    group_sizze_3407 = get_local_size(0);\n    wave_sizze_3406 = LOCKSTEP_WIDTH;\n    group_id_3317 = get_group_id(0);\n    thread_active_3408 = 1;\n    \n    int32_t x_3324;\n    char is_first_thread_3338;\n    int32_t result_3342;\n    \n    if (thread_active_3408) {\n        x_3324 = group_id_3317 * y_3323;\n        is_first_thread_3338 = local_tid_3316 == 0;\n        \n        int32_t x_merge_3321 = 0;\n        \n        for (int32_t i_3322 = 0; i_3322 < num_iterations_3320; i_3322++) {\n            int32_t y_3325 = i_3322 * group_sizze_3280;\n            int32_t offset_3326 = x_3324 + y_3325;\n            int32_t j_3327 = offset_3326 + local_tid_3316;\n            char cond_3328 = slt32(j_3327, sizze_3233);\n            int32_t foldres_3331;\n            \n            if (cond_3328) {\n                double xs_elem_3329 = *(__global double *) &xs_mem_3378[j_3327 *\n                                                                        8];\n                double reshape_elem_3330 = *(__global\n                                             double *) &ys_mem_3380[j_3327 * 8];\n       ",
            "         double x_3297 = xs_elem_3329 - 1.0;\n                double x_3298 = fpow64(x_3297, 2.0);\n                double x_3299 = reshape_elem_3330 - 1.0;\n                double y_3300 = fpow64(x_3299, 2.0);\n                double x_3301 = x_3298 + y_3300;\n                char cond_3302 = x_3301 <= 1.0;\n                int32_t partition_incr_3303;\n                \n                if (cond_3302) {\n                    partition_incr_3303 = 1;\n                } else {\n                    partition_incr_3303 = 0;\n                }\n                \n                int32_t zz_3304 = x_merge_3321 + partition_incr_3303;\n                \n                foldres_3331 = zz_3304;\n            } else {\n                foldres_3331 = x_merge_3321;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            if (slt32(local_tid_3316, group_sizze_3280) && 1) {\n                *(__local int32_t *) &mem_3386[local_tid_3316 * 4] =\n                    foldres_3331;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            \n            int32_t my_index_3305;\n            int32_t other_index_3306;\n            int32_t x_3307;\n            int32_t y_3308;\n            int32_t my_index_3411;\n            int32_t other_index_3412;\n            int32_t x_3413;\n            int32_t y_3414;\n            \n            my_index_3305 = local_tid_3316;\n            if (slt32(local_tid_3316, group_sizze_3280)) {\n                y_3308 = *(volatile __local\n                           int32_t *) &mem_3386[local_tid_3316 *\n                                                sizeof(int32_t)];\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                int32_t skip_threads_3416 = 1;\n                \n                while (slt32(skip_threads_3416, 32)) {\n                    if (slt32(local_tid_3316, group_sizze_3280) &&\n                        sle32(skip_threads_3416, local_tid_3316 -\n                              squot32(local_tid_3316, 32) * 32)) {\n         ",
            "               // read operands\n                        {\n                            x_3307 = *(volatile __local\n                                       int32_t *) &mem_3386[(local_tid_3316 -\n                                                             skip_threads_3416) *\n                                                            sizeof(int32_t)];\n                        }\n                        // perform operation\n                        {\n                            int32_t zz_3309 = x_3307 + y_3308;\n                            \n                            y_3308 = zz_3309;\n                        }\n                    }\n                    if (sle32(wave_sizze_3406, skip_threads_3416)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (slt32(local_tid_3316, group_sizze_3280) &&\n                        sle32(skip_threads_3416, local_tid_3316 -\n                              squot32(local_tid_3316, 32) * 32)) {\n                        // write result\n                        {\n                            *(volatile __local\n                              int32_t *) &mem_3386[local_tid_3316 *\n                                                   sizeof(int32_t)] = y_3308;\n                        }\n                    }\n                    if (sle32(wave_sizze_3406, skip_threads_3416)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_3416 *= 2;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // last thread of block 'i' writes its result to offset 'i'\n            {\n                if ((local_tid_3316 - squot32(local_tid_3316, 32) * 32) == 31 &&\n                    slt32(local_tid_3316, group_sizze_3280)) {\n                    *(volatile __local\n                      int32_t *) &mem_3386[squot32(local_tid_3316, 32) *\n                                           sizeof(int32_t)] = y_3308;\n                }\n            }\n      ",
            "      barrier(CLK_LOCAL_MEM_FENCE);\n            // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n            {\n                if (squot32(local_tid_3316, 32) == 0 && slt32(local_tid_3316,\n                                                              group_sizze_3280)) {\n                    y_3414 = *(volatile __local\n                               int32_t *) &mem_3386[local_tid_3316 *\n                                                    sizeof(int32_t)];\n                }\n                // in-block scan (hopefully no barriers needed)\n                {\n                    int32_t skip_threads_3417 = 1;\n                    \n                    while (slt32(skip_threads_3417, 32)) {\n                        if ((squot32(local_tid_3316, 32) == 0 &&\n                             slt32(local_tid_3316, group_sizze_3280)) &&\n                            sle32(skip_threads_3417, local_tid_3316 -\n                                  squot32(local_tid_3316, 32) * 32)) {\n                            // read operands\n                            {\n                                x_3413 = *(volatile __local\n                                           int32_t *) &mem_3386[(local_tid_3316 -\n                                                                 skip_threads_3417) *\n                                                                sizeof(int32_t)];\n                            }\n                            // perform operation\n                            {\n                                int32_t zz_3415 = x_3413 + y_3414;\n                                \n                                y_3414 = zz_3415;\n                            }\n                        }\n                        if (sle32(wave_sizze_3406, skip_threads_3417)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        if ((squot32(local_tid_3316, 32) == 0 &&\n                             slt32(local_tid_3316, group_sizze_3280)) &&\n    ",
            "                        sle32(skip_threads_3417, local_tid_3316 -\n                                  squot32(local_tid_3316, 32) * 32)) {\n                            // write result\n                            {\n                                *(volatile __local\n                                  int32_t *) &mem_3386[local_tid_3316 *\n                                                       sizeof(int32_t)] =\n                                    y_3414;\n                            }\n                        }\n                        if (sle32(wave_sizze_3406, skip_threads_3417)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        skip_threads_3417 *= 2;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // carry-in for every block except the first\n            {\n                if (!(squot32(local_tid_3316, 32) == 0 || !slt32(local_tid_3316,\n                                                                 group_sizze_3280))) {\n                    // read operands\n                    {\n                        x_3307 = *(volatile __local\n                                   int32_t *) &mem_3386[(squot32(local_tid_3316,\n                                                                 32) - 1) *\n                                                        sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t zz_3309 = x_3307 + y_3308;\n                        \n                        y_3308 = zz_3309;\n                    }\n                    // write final result\n                    {\n                        *(volatile __local int32_t *) &mem_3386[local_tid_3316 *\n                                                                sizeof(int32_t)] =\n                            y_3308;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // re",
            "store correct values for first block\n            {\n                if (squot32(local_tid_3316, 32) == 0) {\n                    *(volatile __local int32_t *) &mem_3386[local_tid_3316 *\n                                                            sizeof(int32_t)] =\n                        y_3308;\n                }\n            }\n            if (cond_3328) {\n                int32_t scanned_elem_3335 = *(__local\n                                              int32_t *) &mem_3386[local_tid_3316 *\n                                                                   4];\n                \n                *(__global int32_t *) &mem_3383[j_3327 * 4] = scanned_elem_3335;\n            }\n            \n            int32_t new_carry_3340;\n            \n            if (is_first_thread_3338) {\n                int32_t carry_3339 = *(__local int32_t *) &mem_3386[y_3282 * 4];\n                \n                new_carry_3340 = carry_3339;\n            } else {\n                new_carry_3340 = 0;\n            }\n            \n            int32_t x_merge_tmp_3410 = new_carry_3340;\n            \n            x_merge_3321 = x_merge_tmp_3410;\n        }\n        result_3342 = x_merge_3321;\n    }\n    if (local_tid_3316 == 0) {\n        *(__global int32_t *) &mem_3392[group_id_3317 * 4] = result_3342;\n    }\n}\n__kernel void scan2_kernel_3348(__local volatile int64_t *mem_aligned_0,\n                                int32_t num_groups_3286, __global\n                                unsigned char *mem_3392, __global\n                                unsigned char *mem_3398)\n{\n    __local volatile char *restrict mem_3395 = mem_aligned_0;\n    int32_t wave_sizze_3418;\n    int32_t group_sizze_3419;\n    char thread_active_3420;\n    int32_t global_tid_3348;\n    int32_t local_tid_3349;\n    int32_t group_id_3350;\n    \n    global_tid_3348 = get_global_id(0);\n    local_tid_3349 = get_local_id(0);\n    group_sizze_3419 = get_local_size(0);\n    wave_sizze_3418 = LOCKSTEP_WIDTH;\n    group_id_3350 = get_group_id(0);\n    thread_active_",
            "3420 = 1;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_3349, num_groups_3286) && 1) {\n        int32_t offsets_group_sums_elem_3351 = *(__global\n                                                 int32_t *) &mem_3392[local_tid_3349 *\n                                                                      4];\n        \n        *(__local int32_t *) &mem_3395[local_tid_3349 * 4] =\n            offsets_group_sums_elem_3351;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t my_index_3343;\n    int32_t other_index_3344;\n    int32_t x_3345;\n    int32_t y_3346;\n    int32_t my_index_3421;\n    int32_t other_index_3422;\n    int32_t x_3423;\n    int32_t y_3424;\n    \n    my_index_3343 = local_tid_3349;\n    if (slt32(local_tid_3349, num_groups_3286)) {\n        y_3346 = *(volatile __local int32_t *) &mem_3395[local_tid_3349 *\n                                                         sizeof(int32_t)];\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        int32_t skip_threads_3426 = 1;\n        \n        while (slt32(skip_threads_3426, 32)) {\n            if (slt32(local_tid_3349, num_groups_3286) &&\n                sle32(skip_threads_3426, local_tid_3349 -\n                      squot32(local_tid_3349, 32) * 32)) {\n                // read operands\n                {\n                    x_3345 = *(volatile __local\n                               int32_t *) &mem_3395[(local_tid_3349 -\n                                                     skip_threads_3426) *\n                                                    sizeof(int32_t)];\n                }\n                // perform operation\n                {\n                    int32_t zz_3347;\n                    \n                    if (thread_active_3420) {\n                        zz_3347 = x_3345 + y_3346;\n                    }\n                    y_3346 = zz_3347;\n                }\n            }\n            if (sle32(wave_sizze_3418, skip_threads_3426)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n   ",
            "         if (slt32(local_tid_3349, num_groups_3286) &&\n                sle32(skip_threads_3426, local_tid_3349 -\n                      squot32(local_tid_3349, 32) * 32)) {\n                // write result\n                {\n                    *(volatile __local int32_t *) &mem_3395[local_tid_3349 *\n                                                            sizeof(int32_t)] =\n                        y_3346;\n                }\n            }\n            if (sle32(wave_sizze_3418, skip_threads_3426)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_3426 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if ((local_tid_3349 - squot32(local_tid_3349, 32) * 32) == 31 &&\n            slt32(local_tid_3349, num_groups_3286)) {\n            *(volatile __local int32_t *) &mem_3395[squot32(local_tid_3349,\n                                                            32) *\n                                                    sizeof(int32_t)] = y_3346;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n    {\n        if (squot32(local_tid_3349, 32) == 0 && slt32(local_tid_3349,\n                                                      num_groups_3286)) {\n            y_3424 = *(volatile __local int32_t *) &mem_3395[local_tid_3349 *\n                                                             sizeof(int32_t)];\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            int32_t skip_threads_3427 = 1;\n            \n            while (slt32(skip_threads_3427, 32)) {\n                if ((squot32(local_tid_3349, 32) == 0 && slt32(local_tid_3349,\n                                                               num_groups_3286)) &&\n                    sle32(skip_threads_3427, local_tid_3349 -\n                          squot32(local_tid_3349, 32) * 32)) {\n                    ",
            "// read operands\n                    {\n                        x_3423 = *(volatile __local\n                                   int32_t *) &mem_3395[(local_tid_3349 -\n                                                         skip_threads_3427) *\n                                                        sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        int32_t zz_3425;\n                        \n                        if (thread_active_3420) {\n                            zz_3425 = x_3423 + y_3424;\n                        }\n                        y_3424 = zz_3425;\n                    }\n                }\n                if (sle32(wave_sizze_3418, skip_threads_3427)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if ((squot32(local_tid_3349, 32) == 0 && slt32(local_tid_3349,\n                                                               num_groups_3286)) &&\n                    sle32(skip_threads_3427, local_tid_3349 -\n                          squot32(local_tid_3349, 32) * 32)) {\n                    // write result\n                    {\n                        *(volatile __local int32_t *) &mem_3395[local_tid_3349 *\n                                                                sizeof(int32_t)] =\n                            y_3424;\n                    }\n                }\n                if (sle32(wave_sizze_3418, skip_threads_3427)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_3427 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_3349, 32) == 0 || !slt32(local_tid_3349,\n                                                         num_groups_3286))) {\n            // read operands\n            {\n                x_3345 = *(volatile __local\n                           int32_t *) &mem_3395[(squot32(local_tid_334",
            "9, 32) -\n                                                 1) * sizeof(int32_t)];\n            }\n            // perform operation\n            {\n                int32_t zz_3347;\n                \n                if (thread_active_3420) {\n                    zz_3347 = x_3345 + y_3346;\n                }\n                y_3346 = zz_3347;\n            }\n            // write final result\n            {\n                *(volatile __local int32_t *) &mem_3395[local_tid_3349 *\n                                                        sizeof(int32_t)] =\n                    y_3346;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_3349, 32) == 0) {\n            *(volatile __local int32_t *) &mem_3395[local_tid_3349 *\n                                                    sizeof(int32_t)] = y_3346;\n        }\n    }\n    \n    int32_t scanned_elem_3354;\n    \n    if (thread_active_3420) {\n        scanned_elem_3354 = *(__local int32_t *) &mem_3395[local_tid_3349 * 4];\n    }\n    *(__global int32_t *) &mem_3398[global_tid_3348 * 4] = scanned_elem_3354;\n}\n",
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
    cl_kernel map_kernel_3374;
    int map_kernel_3374_total_runtime;
    int map_kernel_3374_runs;
    cl_kernel scan1_kernel_3315;
    int scan1_kernel_3315_total_runtime;
    int scan1_kernel_3315_runs;
    cl_kernel scan2_kernel_3348;
    int scan2_kernel_3348_total_runtime;
    int scan2_kernel_3348_runs;
    struct opencl_context opencl;
} ;
void setup_opencl_and_load_kernels(struct futhark_context *ctx)
{
    cl_int error;
    cl_program prog = setup_opencl(&ctx->opencl, opencl_program);
    
    {
        ctx->map_kernel_3374 = clCreateKernel(prog, "map_kernel_3374", &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_3374");
    }
    {
        ctx->scan1_kernel_3315 = clCreateKernel(prog, "scan1_kernel_3315",
                                                &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan1_kernel_3315");
    }
    {
        ctx->scan2_kernel_3348 = clCreateKernel(prog, "scan2_kernel_3348",
                                                &error);
        assert(error == 0);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan2_kernel_3348");
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
    ctx->map_kernel_3374_total_runtime = 0;
    ctx->map_kernel_3374_runs = 0;
    ctx->scan1_kernel_3315_total_runtime = 0;
    ctx->scan1_kernel_3315_runs = 0;
    ctx->scan2_kernel_3348_total_runtime = 0;
    ctx->scan2_kernel_3348_runs = 0;
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
    
    cl_int clCreateBuffer_succeeded_3431;
    
    block->mem = clCreateBuffer(ctx->opencl.ctx, CL_MEM_READ_WRITE, size >
                                0 ? size : 1, NULL,
                                &clCreateBuffer_succeeded_3431);
    OPENCL_SUCCEED(clCreateBuffer_succeeded_3431);
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
                "Kernel map_kernel_3374   executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->map_kernel_3374_runs,
                (long) ctx->map_kernel_3374_total_runtime /
                (ctx->map_kernel_3374_runs !=
                 0 ? ctx->map_kernel_3374_runs : 1),
                (long) ctx->map_kernel_3374_total_runtime);
        ctx->total_runtime += ctx->map_kernel_3374_total_runtime;
        ctx->total_runs += ctx->map_kernel_3374_runs;
        fprintf(stderr,
                "Kernel scan1_kernel_3315 executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->scan1_kernel_3315_runs,
                (long) ctx->scan1_kernel_3315_total_runtime /
                (ctx->scan1_kernel_3315_runs !=
                 0 ? ctx->scan1_kernel_3315_runs : 1),
                (long) ctx->scan1_kernel_3315_total_runtime);
        ctx->total_runtime += ctx->scan1_kernel_3315_total_runtime;
        ctx->total_runs += ctx->scan1_kernel_3315_runs;
        fprintf(stderr,
                "Kernel scan2_kernel_3348 executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                ctx->scan2_kernel_3348_runs,
                (long) ctx->scan2_kernel_3348_total_runtime /
                (ctx->scan2_kernel_3348_runs !=
                 0 ? ctx->scan2_kernel_3348_runs : 1),
                (long) ctx->scan2_kernel_3348_total_runtime);
        ctx->total_runtime += ctx->scan2_kernel_3348_total_runtime;
        ctx->total_runs += ctx->scan2_kernel_3348_runs;
        if (ctx->debugging)
            fprintf(stderr, "Ran %d kernels with cumulative runtime: %6ldus\n",
                    ctx->total_runs, ctx->total_runtime);
    }
}
struct futrts_double {
    double v0;
} ;
static struct futrts_double futrts_main(struct futhark_context *ctx,
                                        int64_t xs_mem_sizze_3377,
                                        struct memblock_device xs_mem_3378,
                                        int64_t ys_mem_sizze_3379,
                                        struct memblock_device ys_mem_3380,
                                        int32_t sizze_3233, int32_t sizze_3234);
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
static struct futrts_double futrts_main(struct futhark_context *ctx,
                                        int64_t xs_mem_sizze_3377,
                                        struct memblock_device xs_mem_3378,
                                        int64_t ys_mem_sizze_3379,
                                        struct memblock_device ys_mem_3380,
                                        int32_t sizze_3233, int32_t sizze_3234)
{
    double scalar_out_3405;
    int32_t group_sizze_3280;
    
    group_sizze_3280 = ctx->opencl.cfg.group_size;
    
    int32_t max_num_groups_3281;
    
    max_num_groups_3281 = ctx->opencl.cfg.num_groups;
    
    int32_t y_3282 = group_sizze_3280 - 1;
    int32_t x_3283 = sizze_3233 + y_3282;
    int32_t w_div_group_sizze_3284 = squot32(x_3283, group_sizze_3280);
    int32_t num_groups_maybe_zzero_3285 = smin32(w_div_group_sizze_3284,
                                                 max_num_groups_3281);
    int32_t num_groups_3286 = smax32(1, num_groups_maybe_zzero_3285);
    int32_t num_threads_3287 = num_groups_3286 * group_sizze_3280;
    int64_t binop_x_3382 = sext_i32_i64(sizze_3233);
    int64_t bytes_3381 = binop_x_3382 * 4;
    struct memblock_device mem_3383;
    
    mem_3383.references = NULL;
    memblock_alloc_device(ctx, &mem_3383, bytes_3381);
    
    int32_t y_3318 = num_threads_3287 - 1;
    int32_t x_3319 = sizze_3233 + y_3318;
    int32_t num_iterations_3320 = squot32(x_3319, num_threads_3287);
    int32_t y_3323 = num_iterations_3320 * group_sizze_3280;
    int64_t binop_x_3391 = sext_i32_i64(num_groups_3286);
    int64_t bytes_3390 = binop_x_3391 * 4;
    struct memblock_device mem_3392;
    
    mem_3392.references = NULL;
    memblock_alloc_device(ctx, &mem_3392, bytes_3390);
    
    int64_t binop_y_3385 = sext_i32_i64(group_sizze_3280);
    int64_t bytes_3384 = 4 * binop_y_3385;
    struct memblock_local mem_3386;
    
    mem_3386.references = NULL;
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 0, bytes_3384, NULL));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 1, sizeof(sizze_3233),
                                  &sizze_3233));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 2,
                                  sizeof(num_iterations_3320),
                                  &num_iterations_3320));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 3, sizeof(y_3323),
                                  &y_3323));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 4,
                                  sizeof(xs_mem_3378.mem), &xs_mem_3378.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 5,
                                  sizeof(ys_mem_3380.mem), &ys_mem_3380.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 6,
                                  sizeof(mem_3383.mem), &mem_3383.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan1_kernel_3315, 7,
                                  sizeof(mem_3392.mem), &mem_3392.mem));
    if (1 * (num_groups_3286 * group_sizze_3280) != 0) {
        const size_t global_work_sizze_3433[1] = {num_groups_3286 *
                     group_sizze_3280};
        const size_t local_work_sizze_3437[1] = {group_sizze_3280};
        int64_t time_start_3434, time_end_3435;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan1_kernel_3315");
            fprintf(stderr, "%zu", global_work_sizze_3433[0]);
            fprintf(stderr, "].\n");
            time_start_3434 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                              ctx->scan1_kernel_3315, 1, NULL,
                                              global_work_sizze_3433,
                                              local_work_sizze_3437, 0, NULL,
                                              NULL));
        if (ctx->debugging) {
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
            time_end_3435 = get_wall_time();
            
            long time_diff_3436 = time_end_3435 - time_start_3434;
            
            ctx->scan1_kernel_3315_total_runtime += time_diff_3436;
            ctx->scan1_kernel_3315_runs++;
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan1_kernel_3315",
                    (int) time_diff_3436);
        }
    }
    
    struct memblock_device mem_3398;
    
    mem_3398.references = NULL;
    memblock_alloc_device(ctx, &mem_3398, bytes_3390);
    
    int64_t bytes_3393 = 4 * binop_x_3391;
    struct memblock_local mem_3395;
    
    mem_3395.references = NULL;
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan2_kernel_3348, 0, bytes_3393, NULL));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan2_kernel_3348, 1,
                                  sizeof(num_groups_3286), &num_groups_3286));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan2_kernel_3348, 2,
                                  sizeof(mem_3392.mem), &mem_3392.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->scan2_kernel_3348, 3,
                                  sizeof(mem_3398.mem), &mem_3398.mem));
    if (1 * num_groups_3286 != 0) {
        const size_t global_work_sizze_3438[1] = {num_groups_3286};
        const size_t local_work_sizze_3442[1] = {num_groups_3286};
        int64_t time_start_3439, time_end_3440;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan2_kernel_3348");
            fprintf(stderr, "%zu", global_work_sizze_3438[0]);
            fprintf(stderr, "].\n");
            time_start_3439 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                              ctx->scan2_kernel_3348, 1, NULL,
                                              global_work_sizze_3438,
                                              local_work_sizze_3442, 0, NULL,
                                              NULL));
        if (ctx->debugging) {
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
            time_end_3440 = get_wall_time();
            
            long time_diff_3441 = time_end_3440 - time_start_3439;
            
            ctx->scan2_kernel_3348_total_runtime += time_diff_3441;
            ctx->scan2_kernel_3348_runs++;
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan2_kernel_3348",
                    (int) time_diff_3441);
        }
    }
    
    int32_t num_threads_3373 = w_div_group_sizze_3284 * group_sizze_3280;
    struct memblock_device mem_3401;
    
    mem_3401.references = NULL;
    memblock_alloc_device(ctx, &mem_3401, bytes_3381);
    OPENCL_SUCCEED(clSetKernelArg(ctx->map_kernel_3374, 0, sizeof(sizze_3233),
                                  &sizze_3233));
    OPENCL_SUCCEED(clSetKernelArg(ctx->map_kernel_3374, 1, sizeof(y_3323),
                                  &y_3323));
    OPENCL_SUCCEED(clSetKernelArg(ctx->map_kernel_3374, 2, sizeof(mem_3383.mem),
                                  &mem_3383.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->map_kernel_3374, 3, sizeof(mem_3398.mem),
                                  &mem_3398.mem));
    OPENCL_SUCCEED(clSetKernelArg(ctx->map_kernel_3374, 4, sizeof(mem_3401.mem),
                                  &mem_3401.mem));
    if (1 * (w_div_group_sizze_3284 * group_sizze_3280) != 0) {
        const size_t global_work_sizze_3443[1] = {w_div_group_sizze_3284 *
                     group_sizze_3280};
        const size_t local_work_sizze_3447[1] = {group_sizze_3280};
        int64_t time_start_3444, time_end_3445;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "map_kernel_3374");
            fprintf(stderr, "%zu", global_work_sizze_3443[0]);
            fprintf(stderr, "].\n");
            time_start_3444 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                              ctx->map_kernel_3374, 1, NULL,
                                              global_work_sizze_3443,
                                              local_work_sizze_3447, 0, NULL,
                                              NULL));
        if (ctx->debugging) {
            OPENCL_SUCCEED(clFinish(ctx->opencl.queue));
            time_end_3445 = get_wall_time();
            
            long time_diff_3446 = time_end_3445 - time_start_3444;
            
            ctx->map_kernel_3374_total_runtime += time_diff_3446;
            ctx->map_kernel_3374_runs++;
            fprintf(stderr, "kernel %s runtime: %ldus\n", "map_kernel_3374",
                    (int) time_diff_3446);
        }
    }
    
    int32_t last_index_3271 = sizze_3233 - 1;
    char is_empty_3272 = sizze_3233 == 0;
    int32_t partition_sizze_3273;
    
    if (is_empty_3272) {
        partition_sizze_3273 = 0;
    } else {
        int32_t read_res_3448;
        
        OPENCL_SUCCEED(clEnqueueReadBuffer(ctx->opencl.queue, mem_3401.mem,
                                           CL_TRUE, last_index_3271 * 4,
                                           sizeof(int32_t), &read_res_3448, 0,
                                           NULL, NULL));
        
        int32_t last_offset_3274 = read_res_3448;
        
        partition_sizze_3273 = last_offset_3274;
    }
    
    double res_3275 = sitofp_i32_f64(partition_sizze_3273);
    double res_3276 = sitofp_i32_f64(sizze_3233);
    double y_3277 = res_3275 / res_3276;
    double res_3278 = 4.0 * y_3277;
    
    scalar_out_3405 = res_3278;
    
    struct futrts_double retval_3432;
    
    retval_3432.v0 = scalar_out_3405;
    memblock_unref_device(ctx, &mem_3383);
    memblock_unref_device(ctx, &mem_3392);
    memblock_unref_local(ctx, &mem_3386);
    memblock_unref_device(ctx, &mem_3398);
    memblock_unref_local(ctx, &mem_3395);
    memblock_unref_device(ctx, &mem_3401);
    return retval_3432;
}
struct futhark_f64_1d {
    struct memblock_device mem;
    int64_t shape[1];
} ;
struct futhark_f64_1d *futhark_new_f64_1d(struct futhark_context *ctx,
                                          double *data, int dim0)
{
    struct futhark_f64_1d *arr = malloc(sizeof(struct futhark_f64_1d));
    
    if (arr == NULL)
        return NULL;
    arr->mem.references = NULL;
    memblock_alloc_device(ctx, &arr->mem, dim0 * sizeof(double));
    if (dim0 * sizeof(double) > 0)
        OPENCL_SUCCEED(clEnqueueWriteBuffer(ctx->opencl.queue, arr->mem.mem,
                                            CL_TRUE, 0, dim0 * sizeof(double),
                                            data + 0, 0, NULL, NULL));
    arr->shape[0] = dim0;
    return arr;
}
int futhark_free_f64_1d(struct futhark_context *ctx, struct futhark_f64_1d *arr)
{
    memblock_unref_device(ctx, &arr->mem);
    free(arr);
    return 0;
}
int futhark_values_f64_1d(struct futhark_context *ctx,
                          struct futhark_f64_1d *arr, double *data)
{
    if (arr->shape[0] * sizeof(double) > 0)
        OPENCL_SUCCEED(clEnqueueReadBuffer(ctx->opencl.queue, arr->mem.mem,
                                           CL_TRUE, 0, arr->shape[0] *
                                           sizeof(double), data + 0, 0, NULL,
                                           NULL));
    return 0;
}
int64_t *futhark_shape_f64_1d(struct futhark_context *ctx,
                              struct futhark_f64_1d *arr)
{
    return arr->shape;
}
int futhark_main(struct futhark_context *ctx, double *out0,
                 struct futhark_f64_1d *in0, struct futhark_f64_1d *in1)
{
    int64_t xs_mem_sizze_3377;
    struct memblock_device xs_mem_3378;
    
    xs_mem_3378.references = NULL;
    
    int64_t ys_mem_sizze_3379;
    struct memblock_device ys_mem_3380;
    
    ys_mem_3380.references = NULL;
    
    int32_t sizze_3233;
    int32_t sizze_3234;
    double scalar_out_3405;
    
    xs_mem_3378 = in0->mem;
    xs_mem_sizze_3377 = in0->mem.size;
    sizze_3233 = in0->shape[0];
    ys_mem_3380 = in1->mem;
    ys_mem_sizze_3379 = in1->mem.size;
    sizze_3234 = in1->shape[0];
    
    struct futrts_double ret_3449;
    
    ret_3449 = futrts_main(ctx, xs_mem_sizze_3377, xs_mem_3378,
                           ys_mem_sizze_3379, ys_mem_3380, sizze_3233,
                           sizze_3234);
    scalar_out_3405 = ret_3449.v0;
    *out0 = scalar_out_3405;
    return 0;
}
