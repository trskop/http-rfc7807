# Problem Details for HTTP APIs (RFC7807)

Extensible implementation of [RFC7807 — Problem Details for HTTP APIs
](https://tools.ietf.org/html/rfc7807) in Haskell.

RFC7807 defines HTTP API error responses that are quite informative. Very basic
example of such message could look like:

```
HTTP/1.1 404 Not Found
Transfer-Encoding: chunked
Date: Sun, 01 Nov 2020 22:28:42 GMT
Server: Warp/3.3.13
Content-Type: application/problem+json;charset=utf-8
Content-Length: 251

{
  "type": "https://example.com/docs/error#upload-to-missing-file",
  "title": "File resource doesn't exist",
  "status": 404,
  "detail": "Cannot upload file content to a non-existent file.",
  "documentId": "ae095978-2f7c-47aa-84dd-220be55195a5"
}
```


## Table of Contents

(Links work on GitHub, but not on Hackage.)

* [What does this package do?](#what-does-this-package-do)
* [Why would I want to use this?](#why-would-i-want-to-use-this)
* [When would I want to use something else?](#when-would-i-want-to-use-something-else)
* [Usage examples](#usage-examples)


## What does this package do?

This library provides a data type `Rfc7807Error` that represents such error
responses. It is designed to be extensible and to allow alternative
representation of user defined fields. See module `Network.HTTP.RFC7807` for
documentation.

In addition the package provides basic support for [`servant-server` package
](https://hackage.haskell.org/package/servant-server). See
[`Servant.Server.RFC7807` module documentation
](https://hackage.haskell.org/package/http-rfc7807/docs/Servant-Server-RFC7807.html)
for more information and usage examples.


## Why would I want to use this?

[![Oh my](./doc/nixcraft-tweet-with-obscure-error-response.png)
](https://twitter.com/nixcraft/status/1321898390209273856)

There are many ways common ways how API error responses are structured in HTTP
APIs. Using this one has few advantages:

* It is standardised in [RFC7807 — Problem Details for HTTP APIs
  ](https://tools.ietf.org/html/rfc7807). This can be leveraged in terms of
  documentation, libraries built with it in mind, and not heaving to reinvent
  something that people have put a lot of thought into.

* Gives you a nice template that is still flexible and extensible. This is
  important if we are retrofitting it into an existing API.

* Documented by default. The `type` field is a URL, used correctly can allow us
  to prevent a lot of issues by linking errors with their documentation.

* Increasing level of detail. We have `type`, `title`, then `detail`, and
  finally custom fields. This way we can progressively add more information
  allowing users to easily debug issues.

* Proxy friendly. When proxies are involved, so called layering, it is quite
  easy to loose the original status code. This can also happen with some HTTP
  libraries that do not retain the status code.

* Probably more...

There's an interesting article "[REST API Error Handling - Problem Details
Response by Guy Levin published on 23rd of May, 2018
](https://blog.restcase.com/rest-api-error-handling-problem-details-response/)"
that compares various stiles of error responses AKA problem details responses.


## When would I want to use something else?

This response structure may not serve your needs if it's clashing with your
needs. Some examples of that:

* Your needs are in direct conflict with the [RFC7807 — Problem Details for
  HTTP APIs](https://tools.ietf.org/html/rfc7807) standard. For example, fields
  that are defined by the standard are reserved in your system or protocol
  you're using.

* When RFC7807 messages cannot easily be retrofitted into your API and you need
  to preserve backward compatibility.

* Probably more...


## Usage examples

General usage (not tied to Servant) is documented in [module
`Network.HTTP.RFC7807` in section Usage Examples
](https://hackage.haskell.org/package/http-rfc7807/docs/Network-HTTP-RFC7807.html#usage-examples).

Basic usage with Servant (actually `servant-server`) is documented in
[module `Servant.Server.RFC7807` in section Usage Examples
](https://hackage.haskell.org/package/http-rfc7807/docs/Servant-Server-RFC7807.html#usage-examples).
