// Copyright (C) 2024 National Center for Atmospheric Research
// SPDX-License-Identifier: Apache-2.0
#include <util/config_yaml.h>
#include <yaml-cpp/yaml.h>
#include <fstream>

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

Yaml* yaml_get_node(Yaml* node, const char* key, bool& found)
{
  YAML::Node subnode = (*node)[key];
  found = subnode.IsDefined() && !subnode.IsScalar();
  return new YAML::Node(subnode);
}

void yaml_delete(Yaml* node)
{
  delete node;
}