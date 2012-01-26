package ag.bett.lift

/**
 * ===BHtml - Bettag Html Package for Liftweb===
 *
 * Wrappers for Liftweb's ultra-flexible SHtml and Mapper-ORM. Mainly this is a collection of Liftweb Helpers i found useful in several projects.
 *
 * It adds support for [[http://twitter.github.com/bootstrap Twitter Bootstrap]] CSS-classes for validation, also makes validation for all
 * KeyedMapper-derived Models easy through a [[http://twitter.github.com/bootstrap/javascript.html#popover Twitter BootstrapJS Popover]].
 *
 * ===Source Code===
 * [[https://github.com/fbettag/lift-bhtml]]
 *
 *
 * ===Author===
 *
 * Developed by [[http://twitter.com/fbettag Franz Bettag]]
 *
 * Developer's Blog [[http://www.uberblo.gs Developer Blog]]
 *
 * ==License==
 * {{{
 *  Copyright 2012 Franz Bettag <franz@bett.ag>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * }}}
 *
 */
package object bhtml {

	/** Makes JsCmd into () =&gt; JsCmd */
	implicit def jsCmd2JsFutureFunction(js: net.liftweb.http.js.JsCmd) = { () => js }

	/** Makes JsRaw into () =&gt; JsCmd */
	implicit def jsCmd2JsFutureFunction(js: net.liftweb.http.js.JE.JsRaw) = { () => js.cmd }

	/** Makes () =&gt; JsRaw into () =&gt; JsCmd */
	implicit def jsCmd2JsFutureFunction(js: () => net.liftweb.http.js.JE.JsRaw) = { () => js().cmd }

	/** Casts a String into [[java.net.InetAddress]] using InetUtils */
	implicit def string2InetAddress(addr: String): java.net.InetAddress = InetUtils.getAddress(addr)


}
