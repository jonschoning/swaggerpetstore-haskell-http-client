# swagger-petstore-tests-integration

This contains integration tests for the haskell http-client swagger-petstore api client library.

The integration tests require a swagger petstore server running at
`http://0.0.0.0/v2`, or the value of the `HOST` environment variable.

### Usage

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack --install-ghc test` to run the integration tests.
3. After stack installs ghc on the first run, `--install-ghc` can be omitted

### Environment Variables

* `HOST` - the root url of the petstore server
* `http_proxy` - the address of the http proxy 

Example: 

`HOST=http://0.0.0.0/v2  http_proxy=http://0.0.0.0:8080 stack --install-ghc test`


### Running with Maven

an example command to run the integration tests with maven (in this directory) is:

`mvn -q verify -Pintegration-test`
