# easy-find

Simple searching for files in Emacs, inspired by Nemo file manager.

## Description

`easy-find` provides a convenient interface for searching types of
files in directories and sub-directories using pipe-delimited
patterns, similar to how Nemo handles file searches. Results are
displayed in dired buffers with details hidden for cleaner viewing.
Great for file management.

## Installation

### Manual Installation
1. Download `easy-find.el`
2. Place in your Emacs load path
3. Add to your init file:
```elisp
(require 'easy-find)
```

### MELPA (coming soon)
```elisp
(use-package easy-find
  :ensure t)
```

## Usage

### Basic Search
`M-x easy-find` - Search for files using pipe-delimited patterns

Example patterns:
- `*.org|*.md` - Find all org and markdown files
- `*.jpg|*.png|*.gif` - Find image files
- `report*|summary*` - Find files starting with "report" or "summary"

### Predefined Search Functions

* `easy-find-videos` - Video files
* `easy-find-images` - Image files  
* `easy-find-documents` - Document files
* `easy-find-text` - Text files (md, org, txt)
* `easy-find-org` - Org mode files
* `easy-find-pdf` - PDF files
* `easy-find-txt` - Text files
* `easy-find-md` - Markdown files
* `easy-find-audio` - Audio files
* `easy-find-compressed` - Archive files
* `easy-find-cleanup` - Various file types for cleanup

## Features

- Pipe-delimited search patterns (`*.ext1|*.ext2`)
- Case-sensitive and case-insensitive searching
- Files only (directories excluded)
- Searches subdirectories
- Clean dired display with hidden details
- Predefined searches for common file types
- Easy to customize

## Video

[Watch a video on easy-find](https://youtu.be/wUh-d4a16r4)

## Requirements

- Emacs 24.1+
- find command (standard on Unix systems)

## License

GPL-3.0

## Author

Raoul Comninos
