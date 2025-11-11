#ifndef CHANCE_PREPROC_H
#define CHANCE_PREPROC_H

char *chance_preprocess_source(const char *path, const char *src, int len,
							   int *out_len, const char *target_arch_name);

#endif
