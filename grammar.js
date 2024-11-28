/**
 * @file Luau grammar for Tree-sitter
 * @author teapo <4teapo@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const lua = require("@tree-sitter-grammars/tree-sitter-lua/grammar");

const PREC = {
  OR: 1, // or
  AND: 2, // and
  COMPARE: 3, // < > <= >= ~= ==
  CONCAT: 4, // ..
  ADD: 5, // + -
  MUL: 6, // * / // %
  UNARY: 7, // not # -
  POWER: 8, // ^
  TYPE_ASSERTION: 9, // ::
  TYPE_UNION: 10, // |
  TYPE_INTERSECTION: 11, // &
  TYPE_OPTIONAL: 12, // ?
  TYPE_REFERENCE: 13, // p.q
};

module.exports = grammar(lua, {
  name: "luau",

  conflicts: ($) => [
    [$.type_pack, $.bound_type_list],
    [$.parenthesized_type, $.type_binding],
    [$.type_pack, $.type_binding],
    [$.type_pack, $.type_binding, $.parenthesized_type],
    [$.type_pack, $.parenthesized_type],
  ],

  supertypes: ($) => [
    $.statement,
    $.last_statement,
    $.expression,
    $.declaration,
    $.type,
    $.inner_type,
    $.variable,
  ],

  inline: ($) => [
    $._type_identifier,
    $._field_identifier,
    $._field_separator,
    $._type_list,
    $._table_property_attribute,
  ],

  rules: {
    // chunk = block
    chunk: ($) => seq(optional($.hash_bang_line), optional($._block)),

    // block = {stat [';']} [laststat [';']]
    _block: ($) =>
      choice(
        seq($.last_statement, optional(";")),
        seq(
          repeat1(seq($.statement, optional(";"))),
          optional(seq($.last_statement, optional(";"))),
        ),
      ),

    // laststat = 'return' [explist] | 'break' | 'continue'
    last_statement: ($) =>
      choice($.return_statement, $.break_statement, $.continue_statement),

    // 'break'
    break_statement: (_) => "break",

    // 'return' [explist]
    return_statement: ($) =>
      seq("return", optional(alias($._expression_list, $.expression_list))),

    _type_identifier: ($) => alias($.identifier, $.type_identifier),

    _field_identifier: ($) => alias($.identifier, $.field_identifier),

    _reserved_identifier: ($) => choice("continue", "export", "type", "typeof"),

    // binding = NAME [':' Type]
    binding: ($) =>
      seq(
        field("name", choice($.identifier)),
        optional(seq(":", field("type", $.type))),
      ),

    // bindinglist = binding [',' bindinglist]
    binding_list: ($) => commaSep1($.binding),

    /*
      stat = varlist '=' explist |
          var compoundop exp |
          functioncall |
          'do' block 'end' |
          'while' exp 'do' block 'end' |
          'repeat' block 'until' exp |
          'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end' |
          'for' binding '=' exp ',' exp [',' exp] 'do' block 'end' |
          'for' bindinglist 'in' explist 'do' block 'end' |
          attributes 'function' funcname funcbody |
          attributes 'local' 'function' NAME funcbody |
          'local' bindinglist ['=' explist] |
          ['export'] 'type' NAME ['<' GenericTypeListWithDefaults '>'] '=' Type |
          ['export'] 'type' 'function' NAME funcbody
    */
    statement: ($) =>
      choice(
        $.assignment_statement,
        $.compound_assignment_statement,
        $.function_call,
        $.do_statement,
        $.while_statement,
        $.repeat_statement,
        $.if_statement,
        $.for_statement,
        $.declaration,
      ),

    // binding '=' exp ',' exp [',' exp]
    for_numeric_clause: ($) =>
      seq(
        field("binding", $.binding),
        "=",
        field("start", $.expression),
        ",",
        field("end", $.expression),
        optional(seq(",", field("step", $.expression))),
      ),

    // bindinglist 'in' explist
    for_generic_clause: ($) =>
      seq($.binding_list, "in", alias($._expression_list, $.expression_list)),

    // '...' [':' GenericTypePack | Type]
    variadic_parameter: ($) =>
      seq(
        "...",
        optional(seq(":", field("type", choice($.generic_type_pack, $.type)))),
      ),

    _parameter_list: ($) =>
      choice(
        seq(commaSep1($.binding), optional(seq(",", $.variadic_parameter))),
        $.variadic_parameter,
      ),

    // var = NAME | prefixexp '[' exp ']' | prefixexp '.' NAME
    variable: ($) =>
      choice(
        $.identifier,
        alias($._reserved_identifier, $.identifier),
        $.bracket_index_expression,
        $.dot_index_expression,
      ),

    // prefixexp . NAME
    dot_index_expression: ($) =>
      seq(
        field("table", $._prefix_expression),
        ".",
        field("field", $._field_identifier),
      ),

    // prefixexp ':' NAME
    method_index_expression: ($) =>
      seq(
        field("table", $._prefix_expression),
        ":",
        field("method", $._field_identifier),
      ),

    // field = '[' exp ']' '=' exp | NAME '=' exp | exp
    field: ($) =>
      choice(
        seq(
          "[",
          field("name", $.expression),
          "]",
          "=",
          field("value", $.expression),
        ),
        seq(
          field(
            "name",
            alias(
              choice($._field_identifier, $._reserved_identifier),
              $.field_identifier,
            ),
          ),
          "=",
          field("value", $.expression),
        ),
        field("value", $.expression),
      ),

    // var compoundop exp
    compound_assignment_statement: ($) =>
      seq(
        field("left", $.variable),
        field("operator", $.compound_operator),
        field("right", $.expression),
      ),

    // compoundop :: '+=' | '-=' | '*=' | '/=' | '//=' | '%=' | '^=' | '..='
    compound_operator: ($) =>
      choice("+=", "-=", "*=", "/=", "//=", "%=", "^=", "..="),

    // 'continue'
    continue_statement: (_) => "continue",

    /*
      attributes 'function' funcname funcbody |
      attributes 'local' 'function' NAME funcbody |
      'local' bindinglist ['=' explist] |
      ['export'] 'type' NAME ['<' GenericTypeListWithDefaults '>'] '=' Type |
      ['export'] 'type' 'function' NAME funcbody
    */
    declaration: ($) =>
      choice(
        $.function_declaration,
        $.local_function_declaration,
        $.local_variable_declaration,
        $.type_alias_declaration,
        $.type_function_declaration,
      ),

    // attributes 'function' funcname funcbody
    function_declaration: ($) =>
      seq(
        field("attributes", optional($.attribute_list)),
        "function",
        field("name", $._function_name),
        $._function_body,
      ),

    // attributes 'local' 'function' NAME funcbody
    local_function_declaration: ($) =>
      seq(
        field("attributes", optional($.attribute_list)),
        "local",
        "function",
        field("name", $.identifier),
        $._function_body,
      ),

    _function_name_dot_index_expression: ($) =>
      seq(
        field("table", $._function_name_prefix_expression),
        ".",
        field("field", $._field_identifier),
      ),

    _function_name_method_index_expression: ($) =>
      seq(
        field("table", $._function_name_prefix_expression),
        ":",
        field("method", $._field_identifier),
      ),

    // 'local' bindinglist ['=' explist]
    local_variable_declaration: ($) =>
      seq(
        "local",
        $.binding_list,
        optional(seq("=", alias($._expression_list, $.expression_list))),
      ),

    // pars ::= '(' [litlist] ')' | littable | STRING
    attribute_parameters: ($) =>
      choice(
        seq("(", optional($.literal_list), ")"),
        $.literal_table,
        $.string,
      ),

    // '@[' parattr {',' parattr} ']'
    parameter_attribute: ($) =>
      seq(field("name", $.identifier), optional($.attribute_parameters)),

    // attributes ::= {attribute}
    attribute_list: ($) => repeat1($.attribute),

    // attribute ::= '@' NAME | '@[' parattr {',' parattr} ']'
    attribute: ($) =>
      choice(
        seq(
          "@",
          field(
            "name",
            alias(token.immediate(/[a-zA-Z_][0-9a-zA-Z_]*/), $.identifier),
          ),
        ),
        seq("@", token.immediate("["), commaSep1($.parameter_attribute), "]"),
      ),

    // littable ::= '{' [litfieldlist] '}'
    literal_table: ($) => seq("{", optional($.literal_field_list), "}"),

    // litfieldlist ::= litfield {fieldsep litfield} [fieldsep]
    literal_field_list: ($) =>
      seq($.literal_field, repeat(seq($._field_separator, $.literal_field))),

    // litfield ::= [NAME '='] literal
    literal_field: ($) =>
      seq(
        optional(seq(field("left", $._field_identifier), "=")),
        field("right", $.literal),
      ),

    // literal ::= 'nil' | 'false' | 'true' | NUMBER | STRING | littable
    literal: ($) =>
      choice($.nil, $.true, $.false, $.number, $.string, $.literal_table),

    // litlist ::= literal {',' literal}
    literal_list: ($) => commaSep1($.literal),

    // ['export'] 'type' NAME ['<' GenericTypeListWithDefaults '>'] '=' Type
    type_alias_declaration: ($) =>
      seq(
        optional("export"),
        "type",
        field("name", $._type_identifier),
        optional(
          seq(
            "<",
            field("type_parameters", $.generic_type_list_with_defaults),
            ">",
          ),
        ),
        "=",
        field("type", $.type),
      ),

    // ['export'] 'type' 'function' NAME funcbody
    type_function_declaration: ($) =>
      seq(
        optional("export"),
        "type",
        "function",
        field("name", $._type_identifier),
        field("body", $._function_body),
      ),

    generic_type_parameter: ($) => $._type_identifier,

    // GenericTypeList = NAME [',' GenericTypeList] | GenericTypePackParameter {',' GenericTypePackParameter}
    generic_type_list: ($) =>
      choice(
        seq(
          commaSep1($.generic_type_parameter),
          optional(seq(",", commaSep1($.generic_type_pack_parameter))),
        ),
        commaSep1($.generic_type_pack_parameter),
      ),

    /*
      GenericTypeListWithDefaults =
          NAME ['=' Type] [',' GenericTypeListWithDefaults] |
          GenericTypePackParameterWithDefault {',' GenericTypePackParameterWithDefault}
    */
    generic_type_list_with_defaults: ($) =>
      choice(
        seq(
          commaSep1(
            choice(
              $.generic_type_parameter,
              $.generic_type_parameter_with_default,
            ),
          ),
          optional(
            seq(
              ",",
              commaSep1(
                choice(
                  $.generic_type_pack_parameter,
                  $.generic_type_pack_parameter_with_default,
                ),
              ),
            ),
          ),
        ),
        commaSep1(
          choice(
            $.generic_type_pack_parameter,
            $.generic_type_pack_parameter_with_default,
          ),
        ),
      ),

    // GenericTypePackParameterWithDefault = NAME '...' '=' (TypePack | VariadicTypePack | GenericTypePack)
    generic_type_parameter_with_default: ($) =>
      seq($._type_identifier, "=", field("default", $.type)),

    // GenericTypePackParameter = NAME '...'
    generic_type_pack_parameter: ($) => seq($._type_identifier, "..."),

    // GenericTypePackParameterWithDefault = NAME '...' '=' (TypePack | VariadicTypePack | GenericTypePack)
    generic_type_pack_parameter_with_default: ($) =>
      seq(
        $._type_identifier,
        "...",
        "=",
        field(
          "default",
          choice($.type_pack, $.variadic_type_pack, $.generic_type_pack),
        ),
      ),

    // exp = asexp { binop exp } | unop exp { binop exp }
    expression: ($) =>
      choice(
        $.nil,
        $.false,
        $.true,
        $.number,
        $.string,
        $.vararg_expression,
        $.function_definition,
        $.variable,
        $.function_call,
        $.parenthesized_expression,
        $.table_constructor,
        $.binary_expression,
        $.unary_expression,
        $.if_else_expression,
        $.type_assertion_expression,
        $.interpolated_string,
      ),

    // '...'
    vararg_expression: ($) => "...",

    // attributes 'function' funcbody
    function_definition: ($) =>
      seq(
        field("attributes", optional($.attribute_list)),
        "function",
        $._function_body,
      ),

    // funcbody = ['<' GenericTypeList '>'] '(' [parlist] ')' [':' ReturnType] block 'end'
    _function_body: ($) =>
      seq(
        optional(seq("<", field("type_parameters", $.generic_type_list), ">")),
        field("parameters", $.parameters),
        optional(seq(":", field("return_type", $._return_type))),
        field("body", alias(optional($._block), $.block)),
        "end",
      ),

    // ifelseexp = 'if' exp 'then' exp {'elseif' exp 'then' exp} 'else' exp
    if_else_expression: ($) =>
      seq(
        "if",
        field("condition", $.expression),
        "then",
        field("consequence", $.expression),
        repeat(field("alternative", $.elseif_expression)),
        "else",
        field("alternative", $.expression),
      ),

    // 'elseif' exp 'then' exp
    elseif_expression: ($) =>
      seq(
        "elseif",
        field("condition", $.expression),
        "then",
        field("consequence", $.expression),
      ),

    // asexp = simpleexp ['::' Type]
    type_assertion_expression: ($) =>
      prec.left(
        PREC.TYPE_ASSERTION,
        seq(field("left", $.expression), "::", field("right", $.type)),
      ),

    // GenericTypePack = NAME '...'
    generic_type_pack: ($) =>
      seq(
        choice(
          $._type_identifier,
          alias($._reserved_identifier, $.type_identifier),
        ),
        "...",
      ),

    // VariadicTypePack = '...' Type
    variadic_type_pack: ($) => seq("...", $.type),

    // TypePack = '(' [TypeList] ')'
    type_pack: ($) => seq("(", optional($._type_list), ")"),

    // TypeList = Type [',' TypeList] | '...' Type
    _type_list: ($) =>
      choice(
        //seq(commaSep1($.type), optional(seq(",", "...", $.type))),
        seq(
          commaSep1($.type),
          optional(seq(",", choice($.variadic_type_pack, $.generic_type_pack))),
        ),
        choice($.variadic_type_pack, $.generic_type_pack),
      ),

    // TypeParams = (Type | TypePack | VariadicTypePack | GenericTypePack) [',' TypeParams]
    type_parameters: ($) =>
      commaSep1(
        choice(
          $.type,
          prec.dynamic(1, $.type_pack),
          $.variadic_type_pack,
          $.generic_type_pack,
        ),
      ),

    // fieldsep = ',' | ';'
    _field_separator: ($) => choice(",", ";"),

    type: ($) => choice($.outer_type, $.inner_type),

    outer_type: ($) => seq(choice("|", "&"), $.inner_type),

    inner_type: ($) =>
      choice(
        $.nil,
        $.singleton_type,
        $.type_reference,
        $.typeof_type,
        $.table_type,
        $.function_type,
        $.parenthesized_type,
        $.optional_type,
        $.type_union,
        $.type_intersection,
      ),

    // SingletonType = STRING | 'true' | 'false'
    singleton_type: ($) => choice($.string, $.true, $.false),

    // NAME ['.' NAME] [ '<' [TypeParams] '>' ]
    type_reference: ($) =>
      prec.right(
        PREC.TYPE_REFERENCE,
        seq(
          choice(
            field(
              "name",
              choice(
                $._type_identifier,
                alias($._reserved_identifier, $.type_identifier),
              ),
            ),
            seq(
              field(
                "prefix",
                choice(
                  $.identifier,
                  alias($._reserved_identifier, $.identifier),
                ),
              ),
              ".",
              field(
                "name",
                alias(
                  choice(
                    $._type_identifier,
                    alias($._reserved_identifier, $.type_identifier),
                  ),
                  $.type_identifier,
                ),
              ),
            ),
          ),
          optional(
            seq("<", field("parameters", optional($.type_parameters)), ">"),
          ),
        ),
      ),

    // 'typeof' '(' exp ')'
    typeof_type: ($) => prec(1, seq("typeof", "(", $.expression, ")")),

    // TableType = '{' Type '}' | '{' [PropList] '}'
    table_type: ($) =>
      seq("{", optional(choice($.type, $.table_property_list)), "}"),

    // PropList = TablePropOrIndexer {fieldsep TablePropOrIndexer} [fieldsep]
    table_property_list: ($) =>
      seq(
        sep1(choice($.table_property, $.table_indexer), $._field_separator),
        optional($._field_separator),
      ),

    // TablePropOrIndexer = ['read' | 'write'] (TableProp | TableIndexer)
    // TableProp = NAME ':' Type
    table_property: ($) =>
      seq(
        field(
          "attribute",
          optional(
            alias($._table_property_attribute, $.table_property_attribute),
          ),
        ),
        field(
          "left",
          alias(
            choice($._field_identifier, $._reserved_identifier),
            $.field_identifier,
          ),
        ),
        ":",
        field("right", $.type),
      ),

    // TablePropOrIndexer = ['read' | 'write'] (TableProp | TableIndexer)
    // TableIndexer = '[' Type ']' ':' Type
    table_indexer: ($) =>
      seq(
        field(
          "attribute",
          optional(
            alias($._table_property_attribute, $.table_property_attribute),
          ),
        ),
        "[",
        field("key", $.type),
        "]",
        ":",
        field("value", $.type),
      ),

    // 'read' | 'write'
    _table_property_attribute: ($) =>
      alias(choice("read", "write"), $.identifier),

    // [NAME ':'] Type
    type_binding: ($) =>
      seq(
        optional(
          seq(
            field(
              "name",
              choice($.identifier, alias($._reserved_identifier, $.identifier)),
            ),
            ":",
          ),
        ),
        field("type", $.type),
      ),

    // BoundTypeList = [NAME ':'] Type [',' BoundTypeList] | GenericTypePack | VariadicTypePack
    bound_type_list: ($) =>
      choice(
        seq(
          commaSep1($.type_binding),
          optional(seq(",", choice($.generic_type_pack, $.variadic_type_pack))),
        ),
        $.generic_type_pack,
        $.variadic_type_pack,
      ),

    // ReturnType = Type | TypePack | GenericTypePack | VariadicTypePack
    _return_type: ($) =>
      choice(
        $.type,
        prec.dynamic(1, $.type_pack),
        $.generic_type_pack,
        $.variadic_type_pack,
      ),

    // FunctionType = ['<' GenericTypeList '>'] '(' [BoundTypeList] ')' '->' ReturnType
    function_type: ($) =>
      prec.right(
        seq(
          optional(seq("<", $.generic_type_list, ">")),
          "(",
          field("parameters", optional($.bound_type_list)),
          ")",
          "->",
          field("return_type", $._return_type),
        ),
      ),

    // '(' Type ')'
    parenthesized_type: ($) => seq("(", $.type, ")"),

    // Union = [SimpleType {'?'}] {'|' SimpleType {'?'}}
    // Intersection = [SimpleType] {'&' SimpleType}
    // Type = Union | Intersection
    optional_type: ($) => prec(PREC.TYPE_OPTIONAL, seq($.inner_type, "?")),

    // Union = [SimpleType {'?'}] {'|' SimpleType {'?'}}
    type_union: ($) =>
      prec.left(
        PREC.TYPE_UNION,
        seq(field("left", $.inner_type), "|", field("right", $.inner_type)),
      ),

    // Intersection = [SimpleType] {'&' SimpleType}
    type_intersection: ($) =>
      prec.left(
        PREC.TYPE_INTERSECTION,
        seq(field("left", $.inner_type), "&", field("right", $.inner_type)),
      ),

    interpolated_string: ($) =>
      seq(
        "`",
        repeat(
          choice(
            field(
              "content",
              alias($._interpolation_string_content, $.string_content),
            ),
            $._escape_sequence,
            $.string_interpolation,
          ),
        ),
        "`",
      ),

    _escape_sequence: ($) =>
      choice(
        prec(2, token.immediate(seq("\\", /[^abfnrtvxu'\"`\\\?]/))),
        prec(1, $.escape_sequence),
      ),

    escape_sequence: (_) =>
      token.immediate(
        seq(
          "\\",
          choice(
            /[^xu0-7]/,
            /[0-7]{1,3}/,
            /x[0-9a-fA-F]{2}/,
            /u[0-9a-fA-F]{4}/,
            /u\{[0-9a-fA-F]+\}/,
            /U[0-9a-fA-F]{8}/,
          ),
        ),
      ),

    _interpolation_string_content: (_) =>
      choice(token.immediate(prec(1, /[^`\{\\]+/)), "\\{"),

    string_interpolation: ($) => seq("{", $.expression, "}"),

    // Luau has hex and binary numbers, with the 0x and 0b prefixes, and
    // numbers can contain underscores.
    number: ($) => {
      const decimal_digits = /[0-9][0-9_]*/;
      const signed_integer = seq(optional(choice("-", "+")), decimal_digits);
      const decimal_exponent_part = seq(choice("e", "E"), signed_integer);

      const hex_digits = /[a-fA-F0-9][a-fA-F0-9_]*/;
      const hex_exponent_part = seq(choice("p", "P"), signed_integer);

      const binary_digits = /[0-1][0-1_]*/;

      const decimal_literal = choice(
        seq(
          decimal_digits,
          ".",
          optional(decimal_digits),
          optional(decimal_exponent_part),
        ),
        seq(".", decimal_digits, optional(decimal_exponent_part)),
        seq(decimal_digits, optional(decimal_exponent_part)),
      );

      const hex_literal = seq(
        choice("0x", "0X"),
        hex_digits,
        optional(seq(".", hex_digits)),
        optional(hex_exponent_part),
      );

      const binary_literal = seq(choice("0b", "0B"), binary_digits);

      return token(choice(decimal_literal, hex_literal, binary_literal));
    },

    // exp binop exp
    binary_expression: ($) =>
      choice(
        ...[
          ["or", PREC.OR],
          ["and", PREC.AND],
          ["<", PREC.COMPARE],
          ["<=", PREC.COMPARE],
          ["==", PREC.COMPARE],
          ["~=", PREC.COMPARE],
          [">=", PREC.COMPARE],
          [">", PREC.COMPARE],
          ["+", PREC.ADD],
          ["-", PREC.ADD],
          ["*", PREC.MUL],
          ["/", PREC.MUL],
          ["//", PREC.MUL],
          ["%", PREC.MUL],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(
              field("left", $.expression),
              operator,
              field("right", $.expression),
            ),
          ),
        ),
        ...[
          ["..", PREC.CONCAT],
          ["^", PREC.POWER],
        ].map(([operator, precedence]) =>
          prec.right(
            precedence,
            seq(
              field("left", $.expression),
              operator,
              field("right", $.expression),
            ),
          ),
        ),
      ),

    // unop exp
    unary_expression: ($) =>
      prec.left(
        PREC.UNARY,
        seq(choice("not", "#", "-"), field("operand", $.expression)),
      ),

    // NAME
    identifier: (_) => {
      // Luau identifiers can contain unicode characters. We allow any
      // segment that doesn't include any character which cannot exist
      // in an identifier.
      const identifier_start =
        /[^\p{Control}\s\n\r+\-*/%^#&~|<>=(){}@\[\];:,.\\'"`?\d]/;
      const identifier_continue =
        /[^\p{Control}\s\n\r+\-*/%^#&~|<>=(){}@\[\];:,.\\'"`?]*/;
      return token(seq(identifier_start, identifier_continue));
    },
  },
});

/**
 * Creates a rule to match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @return {SeqRule}
 *
 */
function commaSep1(rule) {
  return sep1(rule, ",");
}

/**
 * Creates a rule to match zero or more of the rules separated by a comma
 * @param {Rule} rule
 * @return {ChoiceRule}
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}

/**
 * Creates a rule to match one or more occurrences of `rule` separated by `sep`
 *
 * @param {RegExp|Rule|String} rule
 *
 * @param {RegExp|Rule|String} sep
 *
 * @return {SeqRule}
 *
 */
function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
