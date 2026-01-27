# MINDSET ACT-UP integration

This is our current effort at hooking ACT-UP models to MINDSET.

When run this starts a Lisp process that listens for HTTP messages (currently on port 9899, though
that can obviously be easily changed), turns the cranks on an ACT-UP model, and returns their result as
the response as a JSON. You can manually send a message to the server as follows: $ curl -d @sample.json http://localhost:9899/predict or curl -d @pilot-em-input.json http://localhost:9899/predict

The messages read should contain a single JSON object with the following sample structure:

{
  "nodes": {
    "code_id": "e8c485e5-410f-4a03-b5c5-c70cbd16912a",
    "embeddings": [
      {
        "code_element_id": "6b42f090-42a5-4774-9a30-4ef487ea1dea",
        "embedding_id": "82d29237-536b-44c9-97a8-eb0af4d0e6b7",
        "address_begin": "1",
        "address_end": "2",
        "labels": ["234234", "abcd"],
        "vector": [2.3, 4.5, 6.7]
      },
      {
        "code_element_id": "d4d45bdb-67e2-431e-b8e7-8d1d1fec2e87",
        "embedding_id": "bd7d8e54-59c0-4f01-a39b-3d8682b14f84",
        "address_begin": "3",
        "address_end": "4",
        "labels": ["2342hjkhj34"],
        "vector": [2.3, 4.5, 7.7]
      },
      {
        "code_element_id": "c1a6543a-9b43-4b83-af50-8c585d3b5883",
        "embedding_id": "46aa8e72-cab1-4910-9dd1-ad388e8c0890",
        "address_begin": "5",
        "address_end": "6",
        "labels": ["23423ertre4", "jdjdjdabcd"],
        "vector": [2.3, 4.5, 8.7]
      },
      {
        "code_element_id": "3cbb004d-bd14-40d7-b352-93c60b74f3d0",
        "embedding_id": "4a74357c-70e0-4ba2-ab2c-dd9cc30e8d4c",
        "address_begin": "8",
        "address_end": "8",
        "labels": ["2sdfs34234", "abcdsdsdsdfsdf"],
        "vector": [2.3, 4.5, 9.7]
      }
    ]
  }
}

The output will be the same JSON as above with the added graph structure:
{
  "code_id": "e8c485e5-410f-4a03-b5c5-c70cbd16912a",
  "nodes": {
    "code_id": "e8c485e5-410f-4a03-b5c5-c70cbd16912a",
    "embeddings": [
      {
        "code_element_id": "6b42f090-42a5-4774-9a30-4ef487ea1dea",
        "embedding_id": "82d29237-536b-44c9-97a8-eb0af4d0e6b7",
        "address_begin": "1",
        "address_end": "2",
        "labels": ["234234", "abcd"],
        "vector": [2.3, 4.5, 6.7]
      },
      {
        "code_element_id": "d4d45bdb-67e2-431e-b8e7-8d1d1fec2e87",
        "embedding_id": "bd7d8e54-59c0-4f01-a39b-3d8682b14f84",
        "address_begin": "3",
        "address_end": "4",
        "labels": ["2342hjkhj34"],
        "vector": [2.3, 4.5, 7.7]
      },
      {
        "code_element_id": "c1a6543a-9b43-4b83-af50-8c585d3b5883",
        "embedding_id": "46aa8e72-cab1-4910-9dd1-ad388e8c0890",
        "address_begin": "5",
        "address_end": "6",
        "labels": ["23423ertre4", "jdjdjdabcd"],
        "vector": [2.3, 4.5, 8.7]
      },
      {
        "code_element_id": "3cbb004d-bd14-40d7-b352-93c60b74f3d0",
        "embedding_id": "4a74357c-70e0-4ba2-ab2c-dd9cc30e8d4c",
        "address_begin": "8",
        "address_end": "8",
        "labels": ["2sdfs34234", "abcdsdsdsdfsdf"],
        "vector": [2.3, 4.5, 9.7]
      }
    ]
  },
  "workflow": {
    "graph": [
      { "src_id": "82d29237-536b-44c9-97a8-eb0af4d0e6b7", "targets": [
        { "dst_id": "bd7d8e54-59c0-4f01-a39b-3d8682b14f84", "probability": 1.0 }] },
      { "src_id": "bd7d8e54-59c0-4f01-a39b-3d8682b14f84", "targets": [
        { "dst_id": "46aa8e72-cab1-4910-9dd1-ad388e8c0890", "probability": 0.3 },
        { "dst_id": "4a74357c-70e0-4ba2-ab2c-dd9cc30e8d4c", "probability": 0.7 }] },
      { "src_id": "46aa8e72-cab1-4910-9dd1-ad388e8c0890", "targets": [
        { "dst_id": "82d29237-536b-44c9-97a8-eb0af4d0e6b7", "probability": 0.1 },
        { "dst_id": "4a74357c-70e0-4ba2-ab2c-dd9cc30e8d4c", "probability": 0.9 }] },
      { "src_id": "4a74357c-70e0-4ba2-ab2c-dd9cc30e8d4c", "targets": [] }
    ]
  }
}

## To Run ##

* ensure that SBCL is installed 

* cd to this directory

* and run `./run-mindset.sh‘

You need the http-server.lisp, ExpertMind.lisp, ExpertMind-helpers.lisp, converted-test-server-data.json, and init_data.lisp files for everything to run appropriately. We have included the run-mindset.sh shell script as a quick way to process the converted-test-server-data CAVA data and produce output compatible with the MINDSET ExpertMind specification. 

## Init file ##

If the file `init_data.lisp`, in this directory, exists it is read. It should contains a single Lisp form,
and list of chunk descriptions that will be used to prime ACT-UP memory. If the file does not exist, a
warning is printed, but everything should proceed smoothly; its use is optional.

Note that an appropriate init file needs to be constructed for each specific user things are operating
upon, it depends upon the underlying graph of things to be highlighted and their IDs. Drew is currently
the doyen of init files, and should be consulted for an appropriate one for a given user.

There are several init files available in the sub-directory `init-files/`. The user id is appended to the 
name of each file and the appropriate one should be selected at startup. These can be copied, or probably
better, symlinked, to `init_data.lisp`. Note that `init_data.lisp` is included in the `.gitignorre` file
so things can be linked or copied there without worrying about clobbering other people’s choices in the repo.

## Hooking up a Lisp function to process the JSON

When the server receives a request it calls the function named `run-model` in the `expert-mind` package, passing it
a Lisp from of the JSON request as its sole argument. Note that the server code itself is isolated in a different package `json-http` (with the abbreviated nickname `jh'), from which are exported `run-standalone`, `jh:start-server` and `jh:stop-server`. When developing a `expert-mind::run-model` function it will typically be most convenient to `load` the single source file `http-server.lisp` and call `jh:start-server` from Lisp.

The Lisp form of JSON is constructed recursively as follows:

* JSON objects are represented by Lisp plists, the keys of which are Lisp keywords.

* The keys are constructed from the JSON key strings by replacing underscores by hyphens and interning their
all upper case version in the `keyword` package

* JSON strings, other than keys in objects, are passed through unchanged

* JSON integers are passed through unchanged

* JSON floating point numbers are passed through as Lisp floats, whose precision is determined by the current value of `*read-default-float-format*`

* JSON lists are converted to Lisp simple vectors; Lisp lists are *not* used because of the ambiguity that could result with plists representing JSON objects

* The JSON Booleans `true` and `false` are converted to the Lisp symbols `t` and `nil`, repsectively

* The JSON `null` value is convert to the Lisp symbol `null`, which is exported from the `common-lisp` package, and so is typically available in all pacakges;
this avoids the ambiguity that would result from represent both JSON `false` and JSON `null` by the same Lisp value `nil`

When performing the inverse transformation on the value returned by `run-model` JSON object keys
are create by taking the print name of the Lisp keyword symbol, downcasing it, and replacing hyphens by underscores.
This corresponds to the convention I believe we have adopted in this project of always using snake_case for such keys
when they are multi-word. Note that if other conventions are used they will typically be corrected converted to Lisp
keywords on input, but to snake_case on output. If we change the convention it will be easy to change the output
form to some other convention, but it must be used uniformly.

If there is no `expert-mind::run-model` function defined a stub version (`jh::default-run-model`) is called instead, which simply returns a constant return value, the example output from the same document referred to above. It will also
write to the log file the Lisp representations of the incoming JSON and constant return value; this may be
useful for testing and/or understanding the Lisp format of the JSON when crafting the `run-model` function.

And this currently has only one query end point. When we want more it should not be difficult to add them.