// Copyright (C) 2024 National Center for Atmospheric Research
// SPDX-License-Identifier: Apache-2.0
#include <util/config_yaml.h>
#include <yaml-cpp/yaml.h>
#include <fstream>
#include <cstring>

Yaml* yaml_create_from_string(const char* yaml_string)
{
  return new YAML::Node(YAML::Load(yaml_string));
}

Yaml* yaml_create_from_file(const char* file_path)
{
  return new YAML::Node(YAML::LoadFile(file_path));
}

void yaml_to_file(Yaml* node, const char* file_path)
{
  std::ofstream file(file_path, std::ofstream::trunc);
  file << *node;
  file.close();
}

int yaml_size(Yaml* node)
{
  return node->size();
}

YamlIterator* yaml_begin(Yaml* node)
{
  return new YAML::iterator(node->begin());
}

YamlIterator* yaml_end(Yaml* node)
{
  return new YAML::iterator(node->end());
}

bool yaml_increment(YamlIterator* iter, YamlIterator* end)
{
  return ++(*iter) != *end;
}

string_t yaml_key(YamlIterator* iter)
{
  string_t string;
  std::string str = (*iter)->first.as<std::string>();
  string.size_ = str.length();
  string.ptr_ = new char[string.size_ + 1];
  strcpy(string.ptr_, str.c_str());
  return string;
}

Yaml* yaml_get_node(Yaml* node, const char* key, bool& found)
{
  YAML::Node subnode = (*node)[key];
  found = subnode.IsDefined() && !subnode.IsScalar();
  return new YAML::Node(subnode);
}

string_t yaml_get_string(Yaml* node, const char* key, bool& found)
{
  found = (*node)[key].IsDefined();
  string_t string;
  if (found) {
    std::string str = (*node)[key].as<std::string>();
    string.size_ = str.length();
    string.ptr_ = new char[string.size_ + 1];
    strcpy(string.ptr_, str.c_str());
    return string;
  }
  string.ptr_ = nullptr;
  string.size_ = 0;
  return string;
}

int yaml_get_int(Yaml* node, const char* key, bool& found)
{
  found = (*node)[key].IsDefined();
  if (found) return (*node)[key].as<int>();
  return 0;
}

float yaml_get_float(Yaml* node, const char* key, bool& found)
{
  found = (*node)[key].IsDefined();
  if (found) return (*node)[key].as<float>();
  return 0.0f;
}

double yaml_get_double(Yaml* node, const char* key, bool& found)
{
  found = (*node)[key].IsDefined();
  if (found) return (*node)[key].as<double>();
  return 0.0;
}

bool yaml_get_bool(Yaml* node, const char* key, bool& found)
{
  found = (*node)[key].IsDefined();
  if (found) return (*node)[key].as<bool>();
  return false;
}

string_array_t yaml_get_string_array(Yaml* node, const char* key, bool& found)
{
  string_array_t array;
  array.size_ = 0;
  array.ptr_ = nullptr;
  YAML::Node array_node = (*node)[key];
  found = array_node.IsDefined();
  if (!found) return array;
  array.size_ = array_node.size();
  array.ptr_ = new string_t[ array.size_ ];
  for (std::size_t i = 0; i < array_node.size(); ++i)
  {
    std::string str = array_node[i].as<std::string>();
    array.ptr_[i].size_ = str.length();
    array.ptr_[i].ptr_ = new char[ str.length() + 1 ];
    strcpy(array.ptr_[i].ptr_, str.c_str());
  }
  return array;
}

void yaml_delete_node(Yaml* ptr)
{
  delete ptr;
}

void yaml_delete_string(string_t string)
{
  delete [] string.ptr_;
}

void yaml_delete_string_array(string_array_t array)
{
  if (!array.ptr_) return;
  for (std::size_t i = 0; i < array.size_; ++i)
  {
    delete [] array.ptr_[i].ptr_;
  }
  delete [] array.ptr_;
}

void yaml_delete_iterator(YamlIterator* ptr)
{
  delete ptr;
}