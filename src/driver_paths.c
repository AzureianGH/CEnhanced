#include "driver_paths.h"

#include "driver_cli.h"

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <direct.h>
#else
#include <limits.h>
#include <unistd.h>
#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif
#endif

#define CHANCECODEC_BASE "chancecodec"
#define CLD_BASE "cld"
#define CHS_BASE "chs"
#ifdef _WIN32
#define CHANCE_PATH_SEP '\\'
#define CHANCECODEC_EXT ".exe"
#define CLD_EXT ".exe"
#define CHS_EXT ".exe"
#else
#define CHANCE_PATH_SEP '/'
#define CHANCECODEC_EXT ""
#define CLD_EXT ""
#define CHS_EXT ""
#endif

void split_path(const char *path, char *dir, size_t dsz,
                char *base_noext, size_t bsz)
{
  if (dir && dsz)
    dir[0] = '\0';
  if (base_noext && bsz)
    base_noext[0] = '\0';
  if (!path)
    return;
  const char *slash1 = strrchr(path, '/');
  const char *slash2 = strrchr(path, '\\');
  const char *base = path;
  const char *dirend = path;
  if (slash1 && slash2)
  {
    const char *mx = (slash1 > slash2 ? slash1 : slash2);
    base = mx + 1;
    dirend = mx;
  }
  else if (slash1)
  {
    base = slash1 + 1;
    dirend = slash1;
  }
  else if (slash2)
  {
    base = slash2 + 1;
    dirend = slash2;
  }
  if (dir && dsz)
  {
    size_t n = (size_t)(dirend - path);
    if (n >= dsz)
      n = dsz - 1;
    memcpy(dir, path, n);
    dir[n] = '\0';
  }
  const char *dot = strrchr(base, '.');
  if (base_noext && bsz)
  {
    size_t n = dot ? (size_t)(dot - base) : strlen(base);
    if (n >= bsz)
      n = bsz - 1;
    memcpy(base_noext, base, n);
    base_noext[n] = '\0';
  }
}

void build_path_with_ext(const char *dir, const char *base,
                         const char *ext, char *buffer, size_t bufsz)
{
#ifdef _WIN32
  const char sep = '\\';
#else
  const char sep = '/';
#endif
  if (!buffer || bufsz == 0)
    return;
  if (!ext)
    ext = "";
  if (dir && dir[0])
    snprintf(buffer, bufsz, "%s%c%s%s", dir, sep, base, ext);
  else
    snprintf(buffer, bufsz, "%s%s", base, ext);
}

int is_regular_file(const char *path)
{
  if (!path || !*path)
    return 0;
  struct stat st;
  if (stat(path, &st) != 0)
    return 0;
#if defined(S_ISREG)
  return S_ISREG(st.st_mode) != 0;
#else
  return (st.st_mode & _S_IFREG) != 0;
#endif
}

int parent_directory(const char *path, char *out, size_t outsz)
{
  if (!out || outsz == 0)
    return 1;
  out[0] = '\0';
  if (!path || !*path)
    return 1;
  size_t len = strlen(path);
  if (len == 0)
    return 1;
  while (len > 0 && (path[len - 1] == '/' || path[len - 1] == '\\'))
    len--;
  while (len > 0 && path[len - 1] != '/' && path[len - 1] != '\\')
    len--;
  while (len > 0 && (path[len - 1] == '/' || path[len - 1] == '\\'))
    len--;
  if (len == 0)
  {
#ifdef _WIN32
    if (path[0] && path[1] == ':')
    {
      out[0] = path[0];
      out[1] = ':';
      out[2] = '\0';
      return 0;
    }
#endif
    return 1;
  }
  if (len >= outsz)
    len = outsz - 1;
  memcpy(out, path, len);
  out[len] = '\0';
  return 0;
}

void strip_wrapping_quotes(const char *in, char *out, size_t outsz)
{
  if (!out || outsz == 0)
    return;
  out[0] = '\0';
  if (!in)
    return;
  while (*in == ' ' || *in == '\t')
    ++in;
  size_t len = strlen(in);
  while (len > 0 && (in[len - 1] == ' ' || in[len - 1] == '\t' ||
                     in[len - 1] == '\n' || in[len - 1] == '\r'))
    --len;
  if (len >= 2)
  {
    if ((in[0] == '"' && in[len - 1] == '"') ||
        (in[0] == '\'' && in[len - 1] == '\''))
    {
      ++in;
      len -= 2;
    }
  }
  if (len >= outsz)
    len = outsz - 1;
  memcpy(out, in, len);
  out[len] = '\0';
}

int get_executable_dir(char *dir, size_t dirsz, const char *argv0)
{
  if (!dir || dirsz == 0)
    return 1;
  dir[0] = '\0';
#ifdef _WIN32
  char resolved[1024];
  if (_fullpath(resolved, argv0 ? argv0 : "", sizeof(resolved)) == NULL)
    return 1;
#else
  char resolved[PATH_MAX];
  resolved[0] = '\0';
  if (!argv0)
    argv0 = "";

  if (!realpath(argv0, resolved))
  {
#if defined(__linux__)
    ssize_t len = readlink("/proc/self/exe", resolved, sizeof(resolved) - 1);
    if (len > 0)
      resolved[len] = '\0';
    else
      resolved[0] = '\0';
#elif defined(__APPLE__)
    uint32_t size = (uint32_t)sizeof(resolved);
    if (_NSGetExecutablePath(resolved, &size) != 0)
    {
      char *dyn = (char *)malloc(size + 1);
      if (dyn)
      {
        if (_NSGetExecutablePath(dyn, &size) == 0 && realpath(dyn, resolved))
        {
          // resolved ok
        }
        else
        {
          if (_NSGetExecutablePath(dyn, &size) == 0)
          {
            strncpy(resolved, dyn, sizeof(resolved) - 1);
            resolved[sizeof(resolved) - 1] = '\0';
          }
          else
          {
            resolved[0] = '\0';
          }
        }
        free(dyn);
      }
      else
      {
        resolved[0] = '\0';
      }
    }
    else
    {
      if (!realpath(resolved, resolved))
      {
        // Keep the unresolved path; it is still useful for locating siblings.
      }
    }
#else
    resolved[0] = '\0';
#endif
  }

  if (!resolved[0] && argv0 && *argv0)
  {
    if (strchr(argv0, '/') || strchr(argv0, '\\'))
    {
      strncpy(resolved, argv0, sizeof(resolved) - 1);
      resolved[sizeof(resolved) - 1] = '\0';
    }
  }

  if (!resolved[0] && argv0 && *argv0)
  {
    const char *path_env = getenv("PATH");
    if (path_env)
    {
      const char *p = path_env;
      while (*p)
      {
        const char *s = p;
        while (*p && *p != ':')
          p++;
        size_t seg_len = (size_t)(p - s);
        if (seg_len > 0)
        {
          char candidate[PATH_MAX];
          if (snprintf(candidate, sizeof(candidate), "%.*s/%s", (int)seg_len, s,
                       argv0) > 0)
          {
            if (access(candidate, X_OK) == 0)
            {
              if (realpath(candidate, resolved))
                break;
              strncpy(resolved, candidate, sizeof(resolved) - 1);
              resolved[sizeof(resolved) - 1] = '\0';
              break;
            }
          }
        }
        if (*p)
          p++;
      }
    }
  }

  if (!resolved[0])
    return 1;
#endif
  split_path(resolved, dir, dirsz, NULL, 0);
  return dir[0] ? 0 : 1;
}

int locate_chancecodec(char *out, size_t outsz, const char *exe_dir)
{
  if (!out || outsz == 0)
    return 1;
  out[0] = '\0';
  const char *env_cmd = getenv("CHANCECODEC_CMD");
  if (!env_cmd || !*env_cmd)
    env_cmd = getenv("CHANCECODEC");
  if (env_cmd && *env_cmd)
  {
    strip_wrapping_quotes(env_cmd, out, outsz);
    if (out[0])
      return 0;
  }
  const char *env_home = getenv("CHANCECODE_HOME");
  if (env_home && *env_home)

  {
    char home_build[1024];
    snprintf(home_build, sizeof(home_build), "%s%cbuild", env_home,
             CHANCE_PATH_SEP);
    build_path_with_ext(home_build, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                        outsz);
    if (is_regular_file(out))
      return 0;
    build_path_with_ext(env_home, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                        outsz);
    if (is_regular_file(out))
      return 0;
  }
  if (exe_dir && *exe_dir)
  {
    build_path_with_ext(exe_dir, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
    char parent[1024];
    if (parent_directory(exe_dir, parent, sizeof(parent)) == 0 && parent[0])
    {
      char sibling[1024];
      snprintf(sibling, sizeof(sibling), "%s%cChanceCode", parent,
               CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                          outsz);
      if (is_regular_file(out))
        return 0;
      snprintf(sibling, sizeof(sibling), "%s%cChanceCode%cbuild", parent,
               CHANCE_PATH_SEP, CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                          outsz);
      if (is_regular_file(out))
        return 0;
      char grandparent[1024];
      if (parent_directory(parent, grandparent, sizeof(grandparent)) == 0 &&
          grandparent[0])
      {
        snprintf(sibling, sizeof(sibling), "%s%cChanceCode", grandparent,
                 CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                            outsz);
        if (is_regular_file(out))
          return 0;
        snprintf(sibling, sizeof(sibling), "%s%cChanceCode%cbuild", grandparent,
                 CHANCE_PATH_SEP, CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                            outsz);
        if (is_regular_file(out))
          return 0;
      }
    }
  }
  out[0] = '\0';
  return 1;
}

int locate_cld(char *out, size_t outsz, const char *exe_dir)
{
  if (!out || outsz == 0)
    return 1;
  out[0] = '\0';
  const char *env_cmd = getenv("CLD_CMD");
  if (!env_cmd || !*env_cmd)
    env_cmd = getenv("CLD");
  if (env_cmd && *env_cmd)
  {
    strip_wrapping_quotes(env_cmd, out, outsz);
    if (out[0])
      return 0;
  }
  const char *env_home = getenv("CLD_HOME");
  if (env_home && *env_home)
  {
    char home_build[1024];
    snprintf(home_build, sizeof(home_build), "%s%cbuild", env_home,
             CHANCE_PATH_SEP);
    build_path_with_ext(home_build, CLD_BASE, CLD_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
    build_path_with_ext(env_home, CLD_BASE, CLD_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
  }
  if (exe_dir && *exe_dir)
  {
    build_path_with_ext(exe_dir, CLD_BASE, CLD_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
    char parent[1024];
    if (parent_directory(exe_dir, parent, sizeof(parent)) == 0 && parent[0])
    {
      char sibling[1024];
      snprintf(sibling, sizeof(sibling), "%s%cCLD", parent, CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CLD_BASE, CLD_EXT, out, outsz);
      if (is_regular_file(out))
        return 0;
      snprintf(sibling, sizeof(sibling), "%s%cCLD%cbuild", parent,
               CHANCE_PATH_SEP, CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CLD_BASE, CLD_EXT, out, outsz);
      if (is_regular_file(out))
        return 0;
      char grandparent[1024];
      if (parent_directory(parent, grandparent, sizeof(grandparent)) == 0 &&
          grandparent[0])
      {
        snprintf(sibling, sizeof(sibling), "%s%cCLD", grandparent,
                 CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CLD_BASE, CLD_EXT, out, outsz);
        if (is_regular_file(out))
          return 0;
        snprintf(sibling, sizeof(sibling), "%s%cCLD%cbuild", grandparent,
                 CHANCE_PATH_SEP, CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CLD_BASE, CLD_EXT, out, outsz);
        if (is_regular_file(out))
          return 0;
      }
    }
  }
  out[0] = '\0';
  return 1;
}

int locate_chs(char *out, size_t outsz, const char *exe_dir)
{
  if (!out || outsz == 0)
    return 1;
  out[0] = '\0';
  const char *env_cmd = getenv("CHS_CMD");
  if (!env_cmd || !*env_cmd)
    env_cmd = getenv("CHS");
  if (env_cmd && *env_cmd)
  {
    strip_wrapping_quotes(env_cmd, out, outsz);
    if (out[0])
      return 0;
  }
  const char *env_home = getenv("CHS_HOME");
  if (env_home && *env_home)
  {
    char home_build[1024];
    snprintf(home_build, sizeof(home_build), "%s%cbuild", env_home,
             CHANCE_PATH_SEP);
    build_path_with_ext(home_build, CHS_BASE, CHS_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
    build_path_with_ext(env_home, CHS_BASE, CHS_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
  }
  if (exe_dir && *exe_dir)
  {
    build_path_with_ext(exe_dir, CHS_BASE, CHS_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
    char parent[1024];
    if (parent_directory(exe_dir, parent, sizeof(parent)) == 0 && parent[0])
    {
      char sibling[1024];
      snprintf(sibling, sizeof(sibling), "%s%cCHS", parent, CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CHS_BASE, CHS_EXT, out, outsz);
      if (is_regular_file(out))
        return 0;
      snprintf(sibling, sizeof(sibling), "%s%cCHS%cbuild", parent,
               CHANCE_PATH_SEP, CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CHS_BASE, CHS_EXT, out, outsz);
      if (is_regular_file(out))
        return 0;
      char grandparent[1024];
      if (parent_directory(parent, grandparent, sizeof(grandparent)) == 0 &&
          grandparent[0])
      {
        snprintf(sibling, sizeof(sibling), "%s%cCHS", grandparent,
                 CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CHS_BASE, CHS_EXT, out, outsz);
        if (is_regular_file(out))
          return 0;
        snprintf(sibling, sizeof(sibling), "%s%cCHS%cbuild", grandparent,
                 CHANCE_PATH_SEP, CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CHS_BASE, CHS_EXT, out, outsz);
        if (is_regular_file(out))
          return 0;
      }
    }
  }
  out[0] = '\0';
  return 1;
}

void trim_whitespace_inplace(char *text)
{
  if (!text)
    return;
  char *start = text;
  while (*start && isspace((unsigned char)*start))
    ++start;
  char *end = start + strlen(start);
  while (end > start && isspace((unsigned char)*(end - 1)))
    --end;
  size_t len = (size_t)(end - start);
  if (start != text && len > 0)
    memmove(text, start, len);
  else if (start != text && len == 0)
    memmove(text, start, 1);
  text[len] = '\0';
}

int parse_bool_value(const char *text, int *out)
{
  if (!text || !out)
    return -1;
  if (equals_icase(text, "true") || equals_icase(text, "1") ||
      equals_icase(text, "yes") || equals_icase(text, "on"))
  {
    *out = 1;
    return 0;
  }
  if (equals_icase(text, "false") || equals_icase(text, "0") ||
      equals_icase(text, "no") || equals_icase(text, "off"))
  {
    *out = 0;
    return 0;
  }
  return -1;
}

int is_path_absolute_simple(const char *path)
{
  if (!path || !*path)
    return 0;
#ifdef _WIN32
  if ((strlen(path) >= 2) && path[1] == ':' &&
      ((path[0] >= 'A' && path[0] <= 'Z') ||
       (path[0] >= 'a' && path[0] <= 'z')))
    return 1;
  if (path[0] == '\\' && path[1] == '\\')
    return 1;
#endif
  return path[0] == '/';
}

void resolve_project_relative_path(char *dst, size_t dstsz,
                                   const char *base_dir,
                                   const char *rel)
{
  if (!dst || dstsz == 0)
    return;
  dst[0] = '\0';
  if (!rel)
    return;
  if (is_path_absolute_simple(rel) || !base_dir || !*base_dir)
  {
    snprintf(dst, dstsz, "%s", rel);
    return;
  }
  size_t base_len = strlen(base_dir);
  int needs_sep = 1;
  if (base_len > 0)
  {
    char last = base_dir[base_len - 1];
    if (last == '/' || last == '\\')
      needs_sep = 0;
  }
  if (needs_sep)
    snprintf(dst, dstsz, "%s%c%s", base_dir, CHANCE_PATH_SEP, rel);
  else
    snprintf(dst, dstsz, "%s%s", base_dir, rel);
}

void normalize_path_simple(char *dst, size_t dstsz, const char *src)
{
  if (!dst || dstsz == 0)
    return;
  dst[0] = '\0';
  if (!src || !*src)
    return;
#ifdef _WIN32
  if (_fullpath(dst, src, dstsz))
    return;
#else
  if (realpath(src, dst))
    return;
#endif
  snprintf(dst, dstsz, "%s", src);
}

const char *get_cwd_path(char *buf, size_t bufsz)
{
  if (!buf || bufsz == 0)
    return NULL;
#ifdef _WIN32
  if (_getcwd(buf, (int)bufsz))
    return buf;
#else
  if (getcwd(buf, bufsz))
    return buf;
#endif
  buf[0] = '\0';
  return NULL;
}
