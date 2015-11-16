Saha is a static website generation and serving tool. Questions, bug reports, and opinions are welcome.

## Features

- **Compact**: You only wanted to set up some static website, but they ask you to install the whole Ruby/Node/etc. tool chain that occupies hundreds of megabytes and takes forever to install. Saha is a single binary executable under two megabytes.
- **Cool URIs**: Concise, meaningful, and maintainable. [W3C has been giving you the idea of what good URIs look like](http://www.w3.org/Provider/Style/URI.html) since decades ago.
- **Correct HTTP headers**: Content charset, Last-Modified and If-Modified-Since, etc.
- **High performance**: Thanks to [Warp](http://www.aosabook.org/en/posa/warp.html).
- Written in **Haskell**.

## Install

Installation is easy!

### x64 Linux

Zero dependency install (except `curl`) which takes only two megabytes:

```sh
curl -L https://github.com/kinoru/saha/releases/latest \
| grep "/saha/releases/download/" | head -n 1 \
| sed -e "s/.*<a href=\"\(.*\)\" rel.*/https:\/\/github.com\1/g" \
| xargs curl -L > ~/.local/bin/saha \
&& chmod +x ~/.local/bin/saha
```

We assume that `~/.local/bin` is in your `$PATH`. The above also works as an update command.

### Other platforms

[Stack](https://github.com/commercialhaskell/stack) is the recommended build tool for Saha.

```sh
$ git clone https://github.com/kinoru/saha.git
$ cd saha
$ stack install
```

The plain old cabal-install should work as well. If it doesn't, please create a new issue and report to us.

## Quickstart

```sh
$ git clone https://github.com/kinoru/saha.git
$ cd saha/example-website
$ saha compile
$ saha server
```

## General flow of setting up a Saha website

1. Create a directory.
1. Have three subdirectories: `data/`, `tpl/`, and `static/`.
1. Put [CommonMark](http://commonmark.org/) documents under `data/`.
1. Put templates under `tpl/`.
1. Put static files under `static/`.
1. Run `saha compile` to create cache by converting CommonMark to HTML and applying templates.
1. Run `saha server` to start the HTTP server.

Use the `example-website/` directory as a reference.

## Directory structure

A Saha website project directory would have the following subdirectories:

- `data/`
- `tpl/`
- `static/`

### Documents

`data/` is where documents are. Documents have the `*.md` file extension.

Below is a typical Saha document.

    title: Main page
    author: Kinoru

    This introductory paragraph is lorem ipsum dolor sit amet.

    ## First chapter

    This is the content of the first chapter.

    ## Second chapter

    This is the content of the second chapter.

A document begins with headers, each having one key and one value. The key and the value are separated by a colon.

Special headers exist: `template`, `publicity`, and `plaintext`.

- `template` is supposed to be omitted. When explicitly set, it determines which template file should be used for templating this document. Otherwise it's `tpl/main.html`.
- `publicity` is also supposed to be omitted. When the value is explicitly set to `hidden`, the document won't be processed at all. This is good for having unfinished documents or deleting documents from the web site without deleting the source file.
- `plaintext` is also supposed to be omitted. When the value is explicitly set to `plaintext`, the CommonMark conversion won't be applied to the content, and the original plaintext will be directly inserted to the `\content\` part of the template.

Any other header (like `title` or `author` in the example) is used for templating.

The body part should be in the [CommonMark](http://commonmark.org/) syntax.

`data/index.md` is treated specially. This is the "main page" of the website, served when requested with `GET /`.

### Templates

Saha comes with a very simple, primitive templating. Templates are in the `tpl/` directory. `tpl/main.html` is the default template.

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

### Static files

Files in the `static/` directory will be served directly.

## Running the server

&hellip; is done with two steps:

- `saha compile`
- `saha server`

`saha compile` reads the source documents, converts CommonMark to HTML, applies the templating to create the cache, and put them under the `output/` directory. `saha server` serves the contents of the `output/` and `static/`.

There is a separate executable `saha-server`. It is (supposed to be) smaller than the full `saha` binary and has less overhead. You are recommended to use this for server deployment.

### Server options

#### `-d` or `--debug`

When running `saha server` without the `-d` or `--debug` option, the [file descriptor cache duration](http://www.yesodweb.com/blog/2012/09/caching-fd) will be set to 60 seconds. Caching file descriptors significantly improves the performance, but it may cause misbehavior if you rapidly change site contents and do the refresh from the browser. Make sure you set `-d` during development.

#### `-s` or `--socket`

`saha server` opens the port 3000 by default. You may override this with `-s` or `--socket` option. Example:

```sh
saha server -h http://example.com
saha server -h http://example.com -s 8080
sudo -u www-data saha server -h http://example.com -s /tmp/haskell-kr.socket
```

#### `-h` or `--host`

The `-h` option determines the host name that Saha will use to generate the redirection URIs. Saha automatically redirects all requests with the trailing slash to a slash-less URI, because [it's considered a better practice](https://googlewebmastercentral.blogspot.com/2010/04/to-slash-or-not-to-slash.html). However [RFC 2616](https://tools.ietf.org/html/rfc2616#section-10.3.2) states that the new location given by HTTP 301 must be an absolute URI and there is no way Saha can figure it out without knowing the absolute host name. For local development testing, omitting `-h` should cause no problem because most web browsers generously handle relative redirects too, but when deploying you are encouraged to give this command line option to make sure the website correctly behaves.

Example:

```sh
saha server -h http://example.com
```

## Todo

- Logging
- Supporting a better "not found" page
- Per-directory templates
- Compressed cache
