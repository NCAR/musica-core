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

/// @brief Create a YAML node from a string
/// @param yaml_string YAML in string form
/// @return pointer to the new YAML node
Yaml* yaml_create_from_string(const char* yaml_string);

/// @brief Create a YAML node from a YAML file
/// @param file_path path to the YAML file
/// @return pointer to the new YAML node
Yaml* yaml_create_from_file(const char* file_path);

/// @brief Output a YAML node to a file
/// @param node YAML node to output
/// @param file_path path to file to create (any existing file will be overwritten) 
void yaml_to_file(Yaml* node, const char* file_path);

/// @brief Return the number of child elements in the node
///        This works for vectors and maps
/// @param node YAML node to return size of
/// @return number of node elements
int yaml_size(Yaml* node);

/// @brief Clean up memory
/// @param node YAML to free memory for
void yaml_delete(Yaml* node);

#ifdef __cplusplus
}
#endif