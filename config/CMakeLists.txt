include (CheckIncludeFiles)

check_include_files (unistd.h HAVE_UNISTD_H)
check_include_files (sys/ioctl.h HAVE_SYS_IOCTL_H)
check_include_files (sys/time.h HAVE_SYS_TIME_H)
check_include_files (sys/types.h HAVE_SYS_TYPES_H)
check_include_files (sys/sysinfo.h HAVE_SYS_SYSINFO_H)
check_include_files (malloc.h HAVE_MALLOC_H)
check_include_files (memory.h HAVE_MEMORY_H)
check_include_files (stdlib.h HAVE_STDLIB_H)
check_include_files (stdio.h HAVE_STDIO_H)
check_include_files (signal.h HAVE_SIGNAL_H)
check_include_files (string.h HAVE_STRING_H)

configure_file (
  "${PROJECT_SOURCE_DIR}/config/yacas_config.h.in"
  "${PROJECT_BINARY_DIR}/include/yacas/yacas_config.h"
  )

configure_file (
  "${PROJECT_SOURCE_DIR}/config/yacas_version.h.in"
  "${PROJECT_BINARY_DIR}/include/yacas/yacas_version.h"
  )

add_definitions(-DHAVE_CONFIG_H)
