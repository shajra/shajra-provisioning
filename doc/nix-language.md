- [About this document](#sec-1)
- [Prerequisites](#sec-2)
- [Primitive literals](#sec-3)
- [Strings](#sec-4)
- [Let-expressions](#sec-5)
- [String interpolation](#sec-6)
- [Functions](#sec-7)
- [Lists](#sec-8)
- [Attribute sets](#sec-9)
- [Paths](#sec-10)
- [Mutable (dangerous) path references](#sec-11)
- [Other (mutable) dangers](#sec-12)
- [Importing](#sec-13)


# About this document<a id="sec-1"></a>

This document is a quick introduction to the Nix programming language.

You can use the [Nix](https://nixos.org/nix) command-line tools without understanding the programming language (also called Nix). However, learning the Nix language, will allow you to develop your own Nix packages and read the source code of others. A lot of this code is in [Nixpkgs](https://github.com/NixOS/nixpkgs), a centralized repository of Nix code for the entire Nix ecosystem.

The Nix community has recently developed [a tutorial for the language](https://nixos.org/guides/nix-language.html), which may be a good alternative or supplement to this document.

This document is no substitute for the [official Nix language documentation](https://nixos.org/manual/nix/stable/language/index.html), which is not that long for a programming language. Nix does not have much syntax relative to other general-purpose programming languages.

# Prerequisites<a id="sec-2"></a>

You can read this document without following along on your own computer.

If you do want to follow along, you need to [install Nix](nix-installation.md). When installing Nix you will need to have the `nix-command` experimental feature enabled to follow along with this document. You won't need `flakes` enabled, though.

# Primitive literals<a id="sec-3"></a>

We can play around with the Nix language with the `nix eval` command. As with many languages, we can use Nix as a simple calculator by passing Nix expressions to it:

```sh
nix eval --expr '1 + 1'
```

    2

Note, we just have to quote our entire expression for a shell invocation. For an interactive session where this quoting isn't needed, you can use the `nix repl` command.

Nix supports a variety of types you'd expect for a programming language, and we get some literal syntax for typical primitives:

```sh
nix eval --expr 'builtins.typeOf 1'
nix eval --expr 'builtins.typeOf 1.0'
nix eval --expr 'builtins.typeOf true'
```

    "int"
    "float"
    "bool"

Though not covered here in great detail, primitives support the typical operations one would expect. For instance, we have `||`, `&&`, and `!` for boolean values. Numeric values have typical arithmetic operators of `+`, `-`, `/`, and `*`. And we can compare all values with `==`.

# Strings<a id="sec-4"></a>

As you may expect from other languages, Nix supports string literals with the conventional double quote syntax:

```sh
nix eval --expr 'builtins.typeOf "hello"'
```

    "string"

Nix also supports multi-line strings with two consecutive single quotes:

```sh
nix eval --expr "''
      line 1
	line 2
    line 3 ''"
```

    "  line 1\n    line 2\nline 3 "

The left-most token in any line establishes a left margin. In the example above, this is `line 3`. Beyond these strings, Nix does not have syntactically significant whitespace.

We concatenate strings with the `+` operator:

```sh
nix eval --expr '"a" + "b"'
```

    "ab"

# Let-expressions<a id="sec-5"></a>

Because of Nix's foundation as a “functional” programming language, you can't repeatedly bind values to variables as you may in other “imperative” languages. When we bind a value to a name, it's permanently bound for the entire scope the name exists within. We manage these scopes of bound names with *let-expressions*:

```sh
nix eval --expr 'let a = 1; b = 2; in a + b'
```

    3

In this example, we've bound `a` to 1, and `b` to 2, but only for the scope of the expression that follows the `in` keyword, `a + b`.

As illustrated below, we can't rebind a name:

```sh
nix eval --expr 'let a = 1; a = 2; in a' 2>&1 || true
```

    error: attribute 'a' already defined at (string):1:5
    
           at «string»:1:12:
    
                1| let a = 1; a = 2; in a
                 |            ^

Note that semicolons are mandatory in all Nix forms that have them, including let-expressions. Because of Nix's strict parsing you can neither elide semicolons, nor put extra ones.

# String interpolation<a id="sec-6"></a>

Sometimes we build up small code snippets inline in a Nix expression, so it's useful to have string interpolation support. Similar to shell scripting, the syntax for this follows:

```sh
nix eval --expr '
    let foo = "Foo";
	bar = "Bar";
    in "${foo + bar} is a terrible name"'
```

    "FooBar is a terrible name"

String interpolation is supported by both normal and multi-line strings.

You can only interpolate strings into strings. For instance, interpolating an integer won't work:

```sh
nix eval --expr '
    let a_number = 42;
    in "${a_number} is a terrible number"' 2>&1 || true
```

    error: cannot coerce an integer to a string
    
           at «string»:3:9:
    
                2|     let a_number = 42;
                3|     in "${a_number} is a terrible number"
                 |         ^

We can use a builtin `toString` function to coerce types to strings:

```sh
nix eval --expr '
    let a_number = 42;
    in "${builtins.toString a_number} is a terrible number"' 2>&1 || true
```

    "42 is a terrible number"

Note that unlike shell scripts, the curly braces are not optional for string interpolation in Nix. This works out in our favor if we're writing shell scripts inline in a Nix expression, because we can use `$name` for shell string interpolation and `${nix_expr}` for Nix string interpolation. If this is not enough, within multiline strings, we can suppress interpolation with by using `''${…}` instead of just `${…}`.

This is a little tedious to illustrate in a shell example without dealing with quote delimiting. In the following example, the shell will interpolate, but not the final Nix expression because we have `''$` instead of just `$` in our final string:

```sh
FROM_SHELL='${NOT_EXPANDED_BY_NIX}'
nix eval --expr "''In ''$FROM_SHELL expansion is prevented.''"
```

    "In \${NOT_EXPANDED_BY_NIX} expansion is prevented."

Here's the same example without the suppression of the interpolation:

```sh
FROM_SHELL='${EXPANDED_BY_NIX}'
nix eval --expr "''In $FROM_SHELL expansion still happens.''" 2>&1 || true
```

    error: undefined variable 'EXPANDED_BY_NIX'
    
           at «string»:1:8:
    
                1| ''In ${EXPANDED_BY_NIX} expansion still happens.''
                 |        ^

# Functions<a id="sec-7"></a>

Nix has first class functions. Nix's functions take in only one argument at a time, and use a colon to separate the parameter name from the body of the function. Furthermore, Nix uses whitespace for function application:

```sh
nix eval --expr 'builtins.typeOf (a: a + 1)'
nix eval --expr '(a: a + 1) 2'
```

    "lambda"
    3

Since functions take only one argument at a time, we encode n-ary functions with functions returning functions:

```sh
nix eval --expr '(a: b: a + b) 1 2'
```

    3

In this case, when we apply `1` to `a: b: a + b`, we get another function. When we apply `2` to this resultant function, we finally get our answer `3`.

If you've heard of *currying a function* in other languages with n-ary functions, you may recognize this technique.

# Lists<a id="sec-8"></a>

Nix also has list literals, which use square brackets and are whitespace-delimited:

```sh
nix eval --expr 'builtins.typeOf [1 2 3 4 5]'
```

    "list"

We can append lists together with the `++` operator:

```sh
nix eval --expr '[1 2] ++ [3 4]'
```

    [ 1 2 3 4 ]

The elements of a list in Nix do not have to be the same type, and lists can be nested:

```sh
nix eval --expr '[1 "hello" [true]]'
```

    [ 1 "hello" [ true ] ]

# Attribute sets<a id="sec-9"></a>

Very importantly, Nix has a kind of map called an *attribute set* that is specialized to have textual indices called *attributes* that index values of arbitrary types. It uses the following syntax:

```sh
nix eval --expr 'builtins.typeOf { a = 1; b = 2; }'
nix eval --expr '{ a = 1; b = 2; }'
nix eval --expr '{ a = 1; b = 2; }.b'
```

    "set"
    { a = 1; b = 2; }
    2

Note, `builtins` is just an attribute set that is in scope by default. And `typeOf` is just an attribute that maps to a function that returns a string indicating the type of the argument.

Often used in Nix expressions, we can overlay sets on top of each other with the `//` operator:

```sh
nix eval --expr '{ a = 1; b = 2; } // { b = 3; c = 4; }'
```

    { a = 1; b = 3; c = 4; }

Additionally, we can prefix set literals with the `rec` keyword to get recursive sets. Recursive sets allow values in a set to reference attributes by name:

```sh
nix eval --expr 'rec { a = b; b = 2; }.a'
```

    2

Without the `rec` keyword, we'd get an error:

```sh
nix eval --expr '{ a = b; b = 2; }.a' 2>&1 || true
```

    error: undefined variable 'b'
    
           at «string»:1:7:
    
                1| { a = b; b = 2; }.a
                 |       ^

If a function accepts an attribute set as an argument, we can have Nix destructure the set as a convenience with the following pattern syntax:

```sh
nix eval --expr '({ a, b }: a + b ) { a = 1; b = 2; }'
```

    3

This basic pattern syntax is rigid, and we can't pass in a attribute set with attributes that don't match the pattern:

```sh
nix eval --expr '({ a }: a + 2 ) { a = 3; b = 4; }' 2>&1 || true
```

    error: anonymous function at (string):1:2 called with unexpected argument 'b'
    
           at «string»:1:1:
    
                1| ({ a }: a + 2 ) { a = 3; b = 4; }
                 | ^
           Did you mean a?

If we want to relax the destructuring to accept sets with other attributes we can use a “&#x2026;” form:

```sh
nix eval --expr '({ a, ...}: a + 2 ) { a = 3; b = 4; }'
```

    5

When destructuring, we can still bind the whole set to a name if we want to using a “@” form.

```sh
nix eval --expr '(s@{ a, b }: a + s.b ) { a = 2; b = 3; }'
```

    5

Attribute sets also support an additional syntactic convenience when pulling in locally bound values as attributes, which comes up a lot in Nix. For example, consider the way we're using `a = a` here:

```sh
nix eval --expr 'let a = 3; in { a = a; }'
```

    { a = 3; }

Rather than worrying about spelling the same name correctly both sides of the ‘=’ for an attribute setting, we can use the `inherit` keyword:

```sh
nix eval --expr 'let a = 3; in { inherit a; }'
```

    { a = 3; }

# Paths<a id="sec-10"></a>

Because the Nix language was designed for building packages, file paths come up frequently in Nix expressions. Nix conveniently has a *path* type, indicated by identifiers with at least one slash:

```sh
nix eval --expr 'builtins.typeOf /some/filepath'
nix eval --expr '/some/filepath'
```

    "path"
    /some/filepath

# Mutable (dangerous) path references<a id="sec-11"></a>

> **WARNING:** This section discusses a language feature of Nix that should be avoided in Nix expressions. Use of this feature can lead to subtle build breakages depending on how you've set the `NIX_PATH` environment variable. This section is included only to explain the feature if you encounter it in other's code.

Up until now, all the Nix expressions we've seen have been purely deterministic. `1 + 1` will always evaluate to `2`. This is a valuable property for a build tool. If a Nix expression describes how to build a package, we want to build it consistently every time.

Unfortunately, Nix has a special environment variable `NIX_PATH` which can provide mutable path references. For expressions that use the syntax described in this section, Nix expressions may reference paths that could change dynamically based on how `NIX_PATH` has been set. Builds relying on this are intrinsically non-deterministic.

`NIX_PATH` is a legacy environment variable, that the ecosystem is slowly working to phase out. To assist with this, the `nix` command requires an `--impure` switch to evaluate an expressions that access mutable paths.

As with `PATH`, the settings within `NIX_PATH` are colon-delimited, with earlier settings taking precedence over later ones. There are two forms for setting `NIX_PATH`:

-   `<name>=<filepath>`
-   `<directory>`

In a Nix expression, we can use an angle bracket syntax to search `NIX_PATH` for an existent file path. Here's an example of using `NIX_PATH` to set the name “temporary” to `/tmp`, which we then access with `<temporary>`:

```sh
NIX_PATH=temporary=/tmp nix eval --impure --expr '<temporary>'
```

    /tmp

If we create some files or folders there:

```sh
mkdir --parents /tmp/some.d/path
```

Then we can access them using our name as a path prefix in our angle brackets:

```sh
NIX_PATH=temporary=/tmp nix eval --impure --expr '<temporary/some.d/path>'
```

    /tmp/some.d/path

If we use the second form for `NIX_PATH`, can specify directories without a name. These directories are then used as candidate prefixes until an existent path if found. For example, we can consider `/tmp` as a path prefix when looking up `some.d/path` to find `/tmp/some.d/path`:

```sh
NIX_PATH=/tmp nix eval --impure --expr '<some.d/path>'
```

    /tmp/some.d/path

Now you know about the angle bracket syntax, but please never use it. It's generally caused the Nix community grief. The Nix community looks bad when a build system advertising deterministic builds fails to do so. There's almost always a better way to accomplish what you might with mutable path references.

# Other (mutable) dangers<a id="sec-12"></a>

There's other ways to have mutable references in Nix, but the angle notation discussed in the prior section is the most common found across various projects and legacy documentation.

For instance, it's possible to use some functions found on the `builtins` set to fetch files from the internet. Here's one such example:

```sh
nix eval --impure --expr '
    builtins.fetchGit {
	url = "https://github.com/NixOS/patchelf";
    }
'
```

    {
      lastModified = 1677229859;
      lastModifiedDate = "20230224091059";
      narHash = "sha256-GPtknkAOxMqPqDbTooqjQ94xvPfBKeoFZLhbtfxLZPM=";
      outPath = "/nix/store/zsilrpg89z8n6b49dvnrlzh45081kp2b-source";
      rev = "65bf3d9dc12d994e73694a49c0813d753071d450";
      revCount = 804;
      shortRev = "65bf3d9";
      submodules = false;
    }

Notice that because we're referencing a URL on the internet that might change `nix eval` forces us to use the `--impure` switch to perform this evaluation.

In general, exercise caution any time calling a nix command with `--impure`.

# Importing<a id="sec-13"></a>

We can import paths. If the path is a file, it's loaded as a Nix expression. If it's a directory, a file called “default.nix” is loaded within it.

The Nixpkgs source code, for example, has a `default.nix` file at its root, so we can import a path directly to it

Here's a small example that creates two files, one that imports the other:

```sh
mkdir nix_example

echo '{ a = 3; }' > nix_example/set.nix
echo '(import ./set.nix).a' > nix_example/default.nix

nix eval --file ./nix_example
```

    3

We also see here use of the `--file` switch with `nix`. This is useful when an expression is saved in a file.
