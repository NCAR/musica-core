// Copyright (C) 2024 National Center for Atmospheric Research
// SPDX-License-Identifier: Apache-2.0
#include <util/config_yaml.h>
#include <yaml-cpp/yaml.h>

Yaml* yaml_create_from_string(const char* yaml_string)
{
  return new YAML::Node(YAML::Load(yaml_string));
}

Yaml* yaml_create_from_file(const char* file_path)
{
  return new YAML::Node(YAML::LoadFile(file_path));
}

void yaml_delete(Yaml* node)
{
  delete node;
}