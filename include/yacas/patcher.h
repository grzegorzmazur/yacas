#ifndef YACAS_PATCHER_H
#define YACAS_PATCHER_H

#include <string>
#include <ostream>

#include "lispenvironment.h"

void PatchLoad(const std::string&, std::ostream&, LispEnvironment&);

#endif

