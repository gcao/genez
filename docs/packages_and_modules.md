# Gene Package and Module System

## Overview

Gene's package and module system provides a flexible way to organize and distribute code. The system supports both structured canonical packages and ad-hoc packages for scripts, with a clear module resolution strategy.

## Core Concepts

### Module
A module is the basic unit of code organization in Gene. A module can be:
- **File Module**: A `.gene` file containing Gene code
- **String Module**: A string containing Gene code (useful for dynamic code loading)

Every module must be associated with a package.

### Package
A package is a collection of related modules. There are two types of packages:

#### 1. Canonical Package
A structured package with explicit metadata and organization:
- **Directory Package**: A directory containing a `package.gene` file in its root
- **Archive Package**: A compressed archive (e.g., `.tar.gz`, `.zip`) containing package contents
- **Database Package**: Package stored in a database (for package managers)

Requirements:
- Must have `package.gene` in the root directory
- Has explicit metadata (name, version, dependencies, etc.)
- Can be published and distributed

#### 2. Ad-hoc Package
A virtual package created on-demand for scripts and quick development:
- Created automatically when running a Gene script
- No `package.gene` file required
- Uses the script's directory as the package root

## Package Resolution

When running a Gene script, the package is determined as follows:

1. Search current working directory for `package.gene`
2. If not found, search parent directories up to filesystem root
3. If found, use that directory as the package root (canonical package)
4. If not found anywhere, create an ad-hoc package with current working directory as root

Example:
```
/home/user/projects/myapp/
├── package.gene          # Found! This is the package root
├── src/
│   ├── main.gene
│   └── utils/
│       └── helpers.gene
└── tests/
    └── test_helpers.gene

# Running from /home/user/projects/myapp/src/utils/:
# Package root: /home/user/projects/myapp/
```

## Module Path Resolution

Module paths can be:

### 1. Absolute Paths
Start with `/` and are resolved from the package root:

```gene
(import /src/utils/helpers)      # From package root
(import /lib/core)               # From package root
```

### 2. Relative Paths
Start with `./` or `../` and are resolved relative to the current module:

```gene
(import ./helpers)               # Same directory
(import ../lib/utils)            # Parent directory
(import ../../common/shared)     # Two levels up
```

### 3. Package Paths
Don't start with `/`, `./`, or `../` and are searched in source directories:

```gene
(import utils/helpers)           # Search in all source paths
(import gene/core)               # Search for gene/core in source paths
```

Resolution order for package paths:
1. Search in each directory listed in `^src-dirs` (from package.gene)
2. Use the first matching module found
3. If not found in any source directory, error

## Package.gene Structure

The `package.gene` file defines package metadata and configuration:

```gene
^name "my-package"
^version "1.0.0"
^description "A Gene package"
^author "Your Name"
^license "MIT"
^repository "https://github.com/user/repo"
^homepage "https://example.com"
^bugs "https://github.com/user/repo/issues"
^keywords ["gene" "example"]

# Source directories (searched for modules)
^src-dirs ["src" "lib"]

# Main module (entry point)
^main "src/main"

# Dependencies
^dependencies {
  ^gene-core "^1.0.0"
  ^http-client "~2.3.0"
}

# Development dependencies
^dev-dependencies {
  ^gene-test "^1.0.0"
}

# Scripts
^scripts {
  ^test "gene test"
  ^build "gene build src/main.gene"
  ^run "gene run src/main.gene"
}

# Package-specific configuration
^config {
  ^output-dir "dist"
  ^test-dir "tests"
}
```

## Import Syntax

```gene
# Import entire module
(import utils/helpers)
(import ./local_module)
(import /absolute/path/to/module)

# Import with alias
(import utils/helpers :as h)

# Import specific items
(import utils/helpers [helper1 helper2])

# Import specific items with rename
(import utils/helpers {helper1 :as h1 helper2 :as h2})

# Import all public items
(import utils/helpers *)

# Conditional import
(import? optional/module)  # Returns nil if not found
```

## Module Structure

A typical module structure:

```gene
# Module: src/utils/math.gene

# Module metadata (optional)
^module-name "math-utils"
^module-version "1.0.0"
^module-exports [add subtract multiply divide]

# Private function (not exported by default)
(fn- validate-number [n]
  (if (number? n)
    n
    (error "Expected number, got" (type n))))

# Public functions (exported)
(fn add [a b]
  (+ (validate-number a) (validate-number b)))

(fn subtract [a b]
  (- (validate-number a) (validate-number b)))

(fn multiply [a b]
  (* (validate-number a) (validate-number b)))

(fn divide [a b]
  (if (== b 0)
    (error "Division by zero")
    (/ (validate-number a) (validate-number b))))

# Module initialization code
(println "Math utils loaded")
```

## Example: Multi-file Project

```
myproject/
├── package.gene
├── src/
│   ├── main.gene
│   ├── core/
│   │   ├── config.gene
│   │   └── database.gene
│   └── utils/
│       ├── string.gene
│       └── math.gene
├── lib/
│   └── third-party/
│       └── json.gene
└── tests/
    ├── test_math.gene
    └── test_string.gene
```

package.gene:
```gene
^name "myproject"
^version "0.1.0"
^src-dirs ["src" "lib"]
^main "src/main"
```

src/main.gene:
```gene
# Absolute imports from package root
(import /src/core/config)
(import /src/core/database)

# Package path imports (searched in src-dirs)
(import utils/string)
(import utils/math)
(import third-party/json)

# Main application logic
(fn main []
  (var config (config/load))
  (var db (database/connect config/db-url))
  ...)
```

src/core/database.gene:
```gene
# Relative import
(import ./config)

# Package path import
(import utils/string)

(fn connect [url]
  ...)
```

## Ad-hoc Package Example

Running a standalone script without package.gene:

```bash
# /tmp/quick-script.gene
(import ./helper)  # Looks for /tmp/helper.gene

(fn main []
  (helper/do-something))
```

When running `gene run /tmp/quick-script.gene`:
1. No package.gene found in /tmp or parent directories
2. Ad-hoc package created with root at /tmp
3. Module resolution works relative to /tmp

## Best Practices

1. **Use canonical packages** for libraries and applications
2. **Use ad-hoc packages** for scripts and prototypes
3. **Prefer package paths** over relative paths for better portability
4. **Use absolute paths** sparingly, only for top-level imports
5. **Organize modules** by functionality in subdirectories
6. **Export explicitly** what should be public API
7. **Document module exports** in module metadata

## Implementation Notes

### Module Cache
- Modules are cached after first load
- Cache key includes full resolved path
- Circular dependencies are detected and reported

### Search Path Priority
1. Relative paths (./,  ../)
2. Absolute paths (/)
3. Package paths (searched in order of src-dirs)

### Module Execution
- Modules are executed once on first import
- Top-level code runs during import
- Functions and values are defined in module namespace
- Exports are made available to importing module