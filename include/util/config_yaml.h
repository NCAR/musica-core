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

/// @brief Interoperatble string type
struct string_t {
  char* ptr_;
  int size_;
};

/// @brief Interoperable array type for strings
struct string_array_t {
  string_t* ptr_;
  int size_;
};

/// @brief Interoperable array type for doubles
struct double_array_t {
  double* ptr_;
  int size_;
};

/// @brief Interoperable array type for YAML nodes
struct node_array_t {
  Yaml** ptr_;
  int size_;
};

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
/// @return key as a c string
string_t yaml_key(YamlIterator* iter);

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
/// @return Pointer to string as const char array
string_t yaml_get_string(Yaml* node, const char* key, bool& found);

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

/// @brief Gets an array of strings from a YAML node
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @return string array
string_array_t yaml_get_string_array(Yaml* node, const char* key, bool& found);

/// @brief Gets an array of doubles from a YAML node
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @return double array
double_array_t yaml_get_double_array(Yaml* node, const char* key, bool& found);

/// @brief Gets an array of YAML nodes from a YAML node
/// @details It is expected that the caller takes ownership of the individual
///          pointers to YAML nodes in the array
/// @param node YAML node
/// @param key key to search for
/// @param found true if successful, false otherwise
/// @return node array
node_array_t yaml_get_node_array(Yaml* node, const char* key, bool& found);

/// @brief Cleans up memory for a YAML node
/// @param ptr Node pointer to free memory for
void yaml_delete_node(Yaml* ptr);

/// @brief Cleans up memory for a char array
/// @param string String to free memory for
void yaml_delete_string(string_t string);

/// @brief Cleans up memory for an array of strings
/// @param array array to free memory for
void yaml_delete_string_array(string_array_t array);

/// @brief Cleans up memory for an array of doubles
/// @param array array to free memory for
void yaml_delete_double_array(double_array_t array);

/// @brief Cleans up memory for an array of YAML nodes
/// @details It is expected that the caller retains ownership of the
///          individual node pointers in the array
/// @param array array to free memory for
void yaml_delete_node_array(node_array_t array);

/// @brief Cleans up memory for a YAML iterator
/// @param ptr Iterator to free memory for
void yaml_delete_iterator(YamlIterator* ptr);

#ifdef __cplusplus
}
#endif