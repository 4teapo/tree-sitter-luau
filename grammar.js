/**
 * @file Luau grammar for Tree-sitter
 * @author teapo
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "luau",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
