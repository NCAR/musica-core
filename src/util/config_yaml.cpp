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

char* yaml_key(YamlIterator* iter, int& size)
{
  std::string str = (*iter)->first.as<std::string>();
  size = str.length();
  char *cstr = new char[size + 1];
  strcpy(cstr, str.c_str());
  return cstr;
}

Yaml* yaml_get_node(Yaml* node, const char* key, bool& found)
{
  YAML::Node subnode = (*node)[key];
  found = subnode.IsDefined() && !subnode.IsScalar();
  return new YAML::Node(subnode);
}

char* yaml_get_string(Yaml* node, const char* key, bool& found, int& size)
{
  found = (*node)[key].IsDefined();
  if (found) {
    std::string str = (*node)[key].as<std::string>();
    size = str.length();
    char *cstr = new char[size + 1];
    strcpy(cstr, str.c_str());
    return cstr;
  }
  size = 0;
  return NULL;
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

void yaml_delete_node(Yaml* ptr)
{
  delete ptr;
}

void yaml_delete_string(char* ptr)
{
  delete [] ptr;
}

void yaml_delete_iterator(YamlIterator* ptr)
{
  delete ptr;
}