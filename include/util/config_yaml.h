// Copyright (C) 2024 National Center for Atmospheric Research
// SPDX-License-Identifier: Apache-2.0
#pragma once

#include <cstddef>

#ifdef __cplusplus
extern "C" {
  namespace YAML {
    class Node;
  }
  typedef YAML::Node Yaml;
#endif

Yaml* yaml_create_from_string(const char* yaml_string);
void yaml_delete(Yaml* node);

#ifdef __cplusplus
}
#endif