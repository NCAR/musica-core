// Copyright (C) 2024 National Center for Atmospheric Research
// SPDX-License-Identifier: Apache-2.0
#pragma once

#include <cstddef>

#ifdef __cplusplus
#include <yaml-cpp/yaml.h>

extern "C" {
  typedef YAML::Node Yaml;
  typedef YAML::iterator YamlIterator;
#endif

/// @brief Creates a YAML node from a string
/// @param yaml_string YAML in string form
/// @return pointer to the new YAML node
Yaml* yaml_create_from_string(const char* yaml_string);

/// @brief Creates a YAML node from a YAML file
/// @param file_path path to the YAML file
/// @return pointer to the new YAML node
Yaml* yaml_create_from_file(const char* file_path);

/// @brief Outputs a YAML node to a file
/// @param node YAML node to output
/// @param file_path path to file to create (any existing file will be overwritten) 
void yaml_to_file(Yaml* node, const char* file_path);

/// @brief Returns the number of child elements in the node
///        This works for vectors and maps
/// @param node YAML node to return size of
/// @return number of node elements
int yaml_size(Yaml* node);

/// @brief Returns an iterator to the first child node
/// @param node YAML node to iterate over
/// @return beginning iterator
YamlIterator* yaml_begin(Yaml* node);

/// @brief Returns an iterator to one element past the last child node
/// @param node YAML node to iterator over
/// @return ending iterator
YamlIterator* yaml_end(Yaml* node);

/// @brief Increments a YAML iterator
/// @param iter YAML iterator to increment
/// @param end YAML iterator one element past end
/// @return true if incremented iter < end, false otherwise
bool yaml_increment(YamlIterator* iter, YamlIterator* end);

/// @brief Returns the key associated with a YAML iterator
/// @param iter YAML iterator to return key for
/// @param size length of the key string
/// @return key as a c string
char* yaml_key(YamlIterator* iter, int& size);

/// @brief Returns a sub-node
/// @param node parent YAML node
/// @param key key to find
/// @param found true if successful, false otherwise
/// @return sub-node
Yaml* yaml_get_node(Yaml* node, const char* key, bool& found);

/// @brief Gets a string from a YAML node
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @param size size of returned string excluding null char terminator
/// @return Pointer to string as const char array
char* yaml_get_string(Yaml* node, const char* key, bool& found, int& size);

/// @brief Gets an integer from a YAML node
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @return integer value
int yaml_get_int(Yaml* node, const char* key, bool& found);

/// @brief Gets a float from a YAML node
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @return float value
float yaml_get_float(Yaml* node, const char* key, bool& found);

/// @brief Gets a double from a YAML node
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @return double value
double yaml_get_double(Yaml* node, const char* key, bool& found);

/// @brief Gets a boolean from a YAML node
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @return boolean value
bool yaml_get_bool(Yaml* node, const char* key, bool& found);

/// @brief Cleans up memory for a YAML node
/// @param ptr Node pointer to free memory for
void yaml_delete_node(Yaml* ptr);

/// @brief Cleans up memory for a char array
/// @param ptr String to free memory for
void yaml_delete_string(char* ptr);

/// @brief Cleans up memory for a YAML iterator
/// @param ptr Iterator to free memory for
void yaml_delete_iterator(YamlIterator* ptr);

#ifdef __cplusplus
}
#endif