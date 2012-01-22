BHtml - Bettag Html Package for Liftweb
==========

It is a collection of wrapper-libraries build around Liftweb. Check the [API Docs](http://fbettag.github.com/lift-bhtml/latest/api/#ag.bett.lift.bhtml.package) for more.


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


