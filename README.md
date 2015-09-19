Plate is a simple web serving tool originally developed to serve the Haskell-KR(<http://haskell.kr/>) website.

## Quickstart

1. Create a directory.
1. Have three subdirectories: `data/`, `tpl/`, and `static/`.
1. Put [CommonMark](http://commonmark.org/) documents under `data/`.
1. Put templates under `tpl/`.
1. Put static files under `static/`.
1. Run `plate-compile` to create cache by converting CommonMark to HTML and applying templates.
1. Run `plate` to start the HTTP server.

## Project goals

- Haskell. (because it was originally for a local Haskell community website)
- Concise, meaninful, and maintainable URIs. [W3C has been giving you the idea of what good URIs look like](http://www.w3.org/Provider/Style/URI.html) since decades ago.
- Correct HTTP headers. (content charset, Last-Modified, etc.)
- Future extensibility.

## Directory structure

A Plate website project directory would have the following subdirectories:

- data/
- tpl/
- static/

## Documents

`data/` is where documents are. Documents have the `*.md` file extension.

Below is a typical Plate document.

    title: Main page
    author: Kinoru

    This introductory paragraph is lorem ipsum dolor sit amet.

    ## First chapter

    This is the content of the first chapter.

    ## Second chapter

    This is the content of the second chapter.

A document begins with headers, each having one key and one value. The key and the value are separated by a colon.

Two special headers exist: `template` and `publicity`.

- `template` is supposed to be omitted. When explicitly set, it determines which template file should be used for templating this document. Otherwise it's `tpl/main.html`.
- `publicity` is also supposed to be omitted. When the value is explicitly set to `hidden`, the document won't be processed at all. This is good for having unfinished documents or deleting documents from the web site without deleting the source file.

Any other header (like `title` or `author` in the example) is used for templating.

The body part should be in the [CommonMark](http://commonmark.org/) syntax.

## Templates

Plate comes with a very simple, primitive templating. Templates are in the `tpl/` directory. `tpl/main.html` is the default template.

An example template:

```html
<!DOCTYPE html>
<html lang="en">
<head>
<title>\title\</title>
<link rel="stylesheet" type="text/css" href="/static/css/main.css" />
<link rel="icon" type="image/x-icon" href="/favicon.ico" />
</head>
<body>
<h1>\title\</h1>
\content\
</body>
</html>
```

Variables in the template are surrounded by backslashes, like `\title\`. Document headers are used to fill these variables. In the above example, `title: Main page` in the document will set the body of `<title>` and `<h1>` to `Main page`.

## Static files

Files in the `static/` directory will be served directly.

## Running the server

&hellip; is done in two phases:

- `plate-compile`
- `plate`

`plate-compile` reads the source documents, converts CommonMark to HTML, applies the templating to create the cache, and put them under the `output/` directory. `plate` serves the contents of the `output/` and `static/`.

`plate` is designed to be used with a reverse proxy, and therefore opens a Unix domain socket. The default path is `/tmp/plate.socket`, and you can change this with `-s` or `--socket` command line option. Example:

```sh
sudo -u www-data plate -s /tmp/haskell-kr.socket
```
