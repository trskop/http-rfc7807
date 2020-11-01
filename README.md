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

This library provides a data type `Rfc7807Error` that represents such error
responses. It is designed to be extensible and to allow alternative
representation of user defined fields.
