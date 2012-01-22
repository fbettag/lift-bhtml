BHtml
==========

It is a wrapper-library build around Liftweb's ultra-flexible SHtml and Mapper-ORM for easy validation.


## How to use

```shell
./sbt package publish-local
```

The basic idea is, to pass fields something like:

```scala
BHtml.text[Long, MapperClass](myMapperRecord.name)
```

A more complex example would be:

```scala
val enableSubmitJs = JsRaw("""$('input[type=submit]').removeAttr('disabled')""")

val disableSubmitJs = JsRaw("""$('input[type=submit]').attr('disabled', 'disabled')""")

BHtml.text[Long, MapperClass](myMapperRecord.name, false, enableSubmitJs, disableSubmitJs)
```


## ToDo

- Scaladoc
- Sample Project


## Thanks

Thanks to everybody in the Lift Community and on [Liftweb Google Groups](http://groups.google.com/group/liftweb).


## License

```
  Copyright (c) 2011, Franz Bettag <franz@bett.ag>
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
     * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY BETTAG SYSTEMS UG ''AS IS'' AND ANY
  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL BETTAG SYSTEMS UG BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

