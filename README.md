# Lapis
Lapis is a treewalk-interpreted programming language written in rust with a rust inspired syntax.

<details>
<summary>Table of Contents</summary>

1. [Examples](#examples)
1. [Installation](#installation)
1. [Usage](#usage)
1. [License](#license)
1. [Roadmap](#Roadmap)

</details>

### Examples
```
fn greet(name) {
    puts("Hello " + name + "! :D")
}
greet("widici")
```
More examples can be found [here](./examples).

### Installation
Installation with Cargo:
```
cargo install --git https://github.com/widici/lapis
```

### Usage
```
lapis <file_path>
```
For more information run the following command:
```
lapis --help
```

### License
The lapis language is distrubuted with the MIT license.<br />
See [LICENSE](LICENSE) for more details.

### Roadmap
- Static type checker
- Transpiler w/ lua target
- Basic OOP (structs)
- Interfaces/mixins
- Lua/C FFI