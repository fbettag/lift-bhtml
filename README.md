BHtml
==========

It is a wrapper-library build around Liftweb's ultra-flexible SHtml and Mapper-ORM for easy validation.

Sample Project is in the works!


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


## Thanks

Thanks to everybody in the Lift Community and on [Liftweb Google Groups](http://groups.google.com/group/liftweb).


## License

```
   Copyright 2012 Franz Bettag <franz@bett.ag>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

```

