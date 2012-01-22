/* {{{
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
*/// }}}

package ag.bett.lift.bhtml

import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.actor._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd

import org.joda.time._
import org.joda.time.format._

import scala.xml._


/**
 * Adds support for [[http://twitter.github.com/bootstrap Twitter Bootstrap]] CSS-classes for validation, also makes validation for all
 * KeyedMapper-derived Models easy through a [[http://twitter.github.com/bootstrap/javascript.html#popover Twitter BootstrapJS Popover]].
 *
 * All selectors are [[http://api.jquery.com/category/selectors/ jQuery Selectors]].
 *
 * The <i>save</i>-parameter makes fields auto-commit/auto-save the record on Event (blur).
 *
 */
object BHtml {

	/**
	 * Delay a JsCmd
	 */
	def delay(d: Int, js: JsCmd) = JsRaw("""window.setTimeout(function() {%s}, %s);""".format(js.toJsCmd, d))

	/**
	 * Hide elements matching the jQuery CSS-Selector
	 */
	def hide(sel: String): JsCmd = JsRaw("""$('%s').hide()""".format(sel))

	/**
	 * Show elements matching the jQuery CSS-Selector
	 */
	def show(sel: String): JsCmd = JsRaw("""$('%s').show()""".format(sel))

	/**
	 * Toggle elements matching the jQuery CSS-Selector
	 */
	def toggle(sel: String): JsCmd = JsRaw("""$('%s').toggle()""".format(sel))

	object Fx {
		/**
		 * Pulsate elements matching the jQuery CSS-Selector
		 */
		def success(sel: String): JsCmd =
			JsRaw("""$('%s').effect('pulsate', {times: 2}, 200)""".format(sel)).cmd

		/**
		 * Pulsate elements matching the jQuery CSS-Selector
		 */
		def failed(sel: String): JsCmd =
			JsRaw("""$('%s').effect('pulsate', {times: 2}, 200)""".format(sel)).cmd

		/**
		 * Render a List[FieldError] as <code><ul><li/></ul></code> and mark elements invalid using the jQuery CSS-Selector
		 */
		def invalid(sel: String, errors: List[FieldError]): JsCmd =
			invalidated(sel, S.??("Error"), <ul>{errors.map(fe => <li>{fe.msg}</li>)}</ul>)

		/**
		 * Add the CSS-Class <i>success</i> and remove <i>error</i>, also repair from popover for elements matching the jQuery CSS-Selector
		 */
		def validated(sel: String): JsCmd =
			JsRaw("""$('%s').removeClass('error').addClass('success').removeAttr('title data-original-title data-content')""".format(sel)).cmd

		/**
		 * Remove the CSS-Class <i>success</i> and add <i>error</i> for elements matching the jQuery CSS-Selector
		 */
		def invalidated(sel: String): JsCmd =
			JsRaw("""$('%s').removeClass('success').addClass('error')""".format(sel)).cmd

		/**
		 * Remove the CSS-Class <i>success</i> and add <i>error</i> for elements matching the jQuery CSS-Selector and also generate a
		 * [[http://twitter.github.com/bootstrap/javascript.html#popover JavaScript Popover]].
		 */
		def invalidated(sel: String, title: String, body: String): JsCmd =
			invalidated(sel, title, Text(body))

		/**
		 * Remove the CSS-Class <i>success</i> and add <i>error</i> for elements matching the jQuery CSS-Selector and also generate a
		 * [[http://twitter.github.com/bootstrap/javascript.html#popover JavaScript Popover]].
		 */
		def invalidated(sel: String, title: String, body: NodeSeq): JsCmd =
			invalidated(sel) & popover(sel, title, body) & delay(2000, JsRaw("""$('%s').popover('hide')""".format(sel)).cmd)

		/**
		 * Reset the CSS-Classes <i>success</i>, <i>error</i> and repair from popover for elements matching the jQuery CSS-Selector
		 */
		def reset(sel: String): JsCmd =
			JsRaw("""$('%s').removeClass('error').removeClass('success').removeAttr('title data-original-title data-content')""".format(sel)).cmd

		/**
		 * Fadeout elements matching the jQuery CSS-Selector
		 */
		def remove(sel: String): JsCmd =
			JsRaw("""$('%s').fadeOut(400).hide(function() { $('%s').remove(); })""".format(sel, sel)).cmd

		/**
		 * Generate a [[http://twitter.github.com/bootstrap/javascript.html#popover JavaScript Popover]]
		 */
		def popover(sel: String, title: String, body: NodeSeq): JsCmd =
			JsRaw("""$('%s').attr('title', "%s").attr('data-content', "%s").popover({ offset: 10, html: true }).popover('show').click(function(e) { e.preventDefault() })"""
				.format(sel, title.replaceAll("\"", "\\\""), body.toString.replaceAll("\"", "\\\""))).cmd

	}


	/**
	 * Generates an ID unique for this record <b>(restrictions apply)</b>
	 * <b>If the MappedField's fieldOwner is NOT SAVED, this will have -1. Be sure to only have one unsaved per Model on your page.</b>
	 */
	def getCssId[K, T <: KeyedMapper[K, T]](a: MappedField[K, T]) = "BF-%s-%s".format(a.uniqueFieldId.open_!, a.fieldOwner.primaryKeyField.is)


	/**
	 * Save a Record
	 */
	def save[K, T <: KeyedMapper[K, T]](a: T, jsSuccess: () => JsCmd = () => Noop, jsFail: () => JsCmd = () => Noop): JsCmd = {
		if (a.validate.length == 0 && a.save) jsSuccess()
		else jsFail() & a.allFields.map(bf => {
			val css = ".BF-%s-%s".format(bf.uniqueFieldId.open_!, a.primaryKeyField.is)
			if (bf.validate.length == 0) Fx.reset(css)
			else Fx.invalid(css, bf.validate)
		})
	}

	/**
	 * Reset validations for a Record
	 */
	def resetValidation[K, T <: KeyedMapper[K, T]](a: T): JsCmd =
		a.allFields.map(bf => Fx.reset(".BF-%s-%s".format(bf.uniqueFieldId.open_!, a.primaryKeyField.is)))


	def checkbox[K, T <: KeyedMapper[K, T]](a: MappedBoolean[T], save: Boolean, jsSuccess: JsCmd): NodeSeq =
		checkbox[K, T](a, save, Empty, jsSuccess)

	def checkbox[K, T <: KeyedMapper[K, T]](a: MappedBoolean[T], save: Boolean, cssSel: String): NodeSeq =
		checkbox[K, T](a, save, Full(cssSel))

	def checkbox[K, T <: KeyedMapper[K, T]](a: MappedBoolean[T], save: Boolean = false, cssSel: Box[String] = Empty, jsSuccess: JsCmd = Noop): NodeSeq = {
		val cssId = getCssId[K, T](a.asInstanceOf[MappedField[K, T]])

		def update(v: Boolean) = {
			a.set(v)
			if (!save) Noop
			else {
				if (a.fieldOwner.save) Fx.success(cssSel openOr("." + cssId)) & jsSuccess
				else Fx.failed(cssSel openOr("." + cssId))
			}
		}

		SHtml.ajaxCheckbox(a.is, update(_), "class" -> cssId)
	}
	

	def text[K, T <: KeyedMapper[K, T]](a: MappedString[T], save: Boolean): NodeSeq =
		text[K, T](a, Empty, save, Empty)

	def text[K, T <: KeyedMapper[K, T]](a: MappedString[T], save: Boolean, cssClass: String): NodeSeq =
		text[K, T](a, Empty, save, Full(cssClass))

	def text[K, T <: KeyedMapper[K, T]](a: MappedString[T], save: Boolean, jsSuccess: JsCmd, jsFail: JsCmd): NodeSeq =
		text[K, T](a, Empty, save, Empty, jsSuccess, jsFail)

	def text[K, T <: KeyedMapper[K, T]](a: MappedString[T], save: Boolean, cssClass: String, jsSuccess: JsCmd, jsFail: JsCmd): NodeSeq =
		text[K, T](a, Empty, save, Full(cssClass), jsSuccess, jsFail)

	def text[K, T <: KeyedMapper[K, T]](a: MappedString[T], value: Box[String] = Empty, save: Boolean = false, cssClass: Box[String] = Empty, jsSuccess: JsCmd = Noop, jsFail: JsCmd = Noop): NodeSeq = {
		val cssId = getCssId[K, T](a.asInstanceOf[MappedField[K, T]])
		val css = cssId + " " + (cssClass match { case Full(s) => " " + s case _ => "" })

		def update(v: String) = {
			a.set(v)
			val errors = a.validate

			// validation went ok
			if (errors.length == 0) Fx.validated("." + cssId) & jsSuccess & {
				// saving went ok
				if (!save) Noop	else if (a.fieldOwner.save) Fx.success("." + cssId)	else Fx.failed("." + cssId)
			} else Fx.invalid("." + cssId, errors) & jsFail
		}
		SHtml.ajaxText(value openOr a.is, update(_), "class" -> css)
	}


	def int[K, T <: KeyedMapper[K, T]](a: MappedInt[T], cssClass: String): NodeSeq =
		int[K, T](a, false, Full(cssClass))

	def int[K, T <: KeyedMapper[K, T]](a: MappedInt[T], save: Boolean, cssClass: String): NodeSeq =
		int[K, T](a, save, Full(cssClass))

	def int[K, T <: KeyedMapper[K, T]](a: MappedInt[T], save: Boolean, cssClass: String, jsSuccess: JsCmd, jsFail: JsCmd): NodeSeq =
		int[K, T](a, save, Full(cssClass), jsSuccess, jsFail)

	def int[K, T <: KeyedMapper[K, T]](a: MappedInt[T], save: Boolean = false, cssClass: Box[String] = Empty, jsSuccess: JsCmd = Noop, jsFail: JsCmd = Noop): NodeSeq = {
		val cssId = getCssId[K, T](a.asInstanceOf[MappedField[K, T]])
		val css = cssId + " " + (cssClass match { case Full(s) => " " + s case _ => "" })

		def saving = Fx.validated("." + cssId) & jsSuccess & (if (!save) Noop else if (a.fieldOwner.save) Fx.success("." + cssId) else Fx.failed("." + cssId))

		def update(v: String): JsCmd =
			try {
				a(v.toInt)
				if (a.validate.length == 0) saving
				else Fx.invalid("." + cssId, a.validate) & jsFail
			} catch { // not a number
				case _ => Fx.invalidated("." + cssId, S.??("Error"), S.??("must.be.a.number")) & jsFail
			}

		SHtml.ajaxText(a.is.toString, update(_), "class" -> css, "pattern" -> "[0-9]+")
	}


	def intOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableInt[T], cssClass: String): NodeSeq =
		intOptional[K, T](a, false, Full(cssClass))

	def intOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableInt[T], save: Boolean, cssClass: String): NodeSeq =
		intOptional[K, T](a, save, Full(cssClass))

	def intOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableInt[T], save: Boolean, cssClass: String, jsSuccess: JsCmd, jsFail: JsCmd): NodeSeq =
		intOptional[K, T](a, save, Full(cssClass), jsSuccess, jsFail)

	def intOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableInt[T], save: Boolean = false, cssClass: Box[String] = Empty, jsSuccess: JsCmd = Noop, jsFail: JsCmd = Noop): NodeSeq = {
		val cssId = getCssId[K, T](a.asInstanceOf[MappedField[K, T]])
		val css = cssId + " " + (cssClass match { case Full(s) => " " + s case _ => "" })

		def saving = Fx.validated("." + cssId) & jsSuccess & (if (!save) Noop else if (a.fieldOwner.save) Fx.success("." + cssId) else Fx.failed("." + cssId))

		def update(v: String): JsCmd = if (v.matches("(|0(\\.0+)?)")) { a(Empty); saving } else
			try {
				a(Full(v.toInt))
				if (a.validate.length == 0) saving
				else Fx.invalid("." + cssId, a.validate) & jsFail
			} catch { // not a number
				case _ => Fx.invalidated("." + cssId, S.??("Error"), S.??("must.be.a.number")) & jsFail
			}

		SHtml.ajaxText(a.is.toString, update(_), "class" -> css, "pattern" -> "[0-9]*")
	}


	def float[K, T <: KeyedMapper[K, T]](a: MappedDecimal[T], cssClass: String): NodeSeq =
		float[K, T](a, false, Full(cssClass))

	def float[K, T <: KeyedMapper[K, T]](a: MappedDecimal[T], save: Boolean, cssClass: String): NodeSeq =
		float[K, T](a, save, Full(cssClass))

	def float[K, T <: KeyedMapper[K, T]](a: MappedDecimal[T], save: Boolean, cssClass: String, jsSuccess: JsCmd, jsFail: JsCmd): NodeSeq =
		float[K, T](a, save, Full(cssClass), jsSuccess, jsFail)

	def float[K, T <: KeyedMapper[K, T]](a: MappedDecimal[T], save: Boolean = false, cssClass: Box[String] = Empty, jsSuccess: JsCmd = Noop, jsFail: JsCmd = Noop): NodeSeq = {
		val cssId = getCssId[K, T](a.asInstanceOf[MappedField[K, T]])
		val css = cssId + " " + (cssClass match { case Full(s) => " " + s case _ => "" })

		def saving = Fx.validated("." + cssId) & jsSuccess & (if (!save) Noop else if (a.fieldOwner.save) Fx.success("." + cssId) else Fx.failed("." + cssId))

		def update(v: String): JsCmd =
			try {
				a(BigDecimal(v.replaceAll(",", ".")))
				if (a.validate.length == 0) saving
				else Fx.invalid("." + cssId, a.validate) & jsFail
			} catch { // not a number
				case _ => Fx.invalidated("." + cssId, S.??("Error"), S.??("must.be.a.number.or.float")) & jsFail
			}

		SHtml.ajaxText(a.is.toString, update(_), "class" -> css, "pattern" -> "[0-9.,]+")
	}


	def floatOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableDecimal[T], cssClass: String): NodeSeq =
		floatOptional[K, T](a, false, Full(cssClass))

	def floatOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableDecimal[T], save: Boolean, cssClass: String): NodeSeq =
		floatOptional[K, T](a, save, Full(cssClass))

	def floatOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableDecimal[T], save: Boolean, cssClass: String, jsSuccess: JsCmd, jsFail: JsCmd): NodeSeq =
		floatOptional[K, T](a, save, Full(cssClass), jsSuccess, jsFail)

	def floatOptional[K, T <: KeyedMapper[K, T]](a: MappedNullableDecimal[T], save: Boolean = false, cssClass: Box[String] = Empty, jsSuccess: JsCmd = Noop, jsFail: JsCmd = Noop): NodeSeq = {
		val cssId = getCssId[K, T](a.asInstanceOf[MappedField[K, T]])
		val css = cssId + " " + (cssClass match { case Full(s) => " " + s case _ => "" })

		def saving = Fx.validated("." + cssId) & jsSuccess & (if (!save) Noop else if (a.fieldOwner.save) Fx.success("." + cssId) else Fx.failed("." + cssId))

		def update(v: String): JsCmd = if (v.matches("(|0(\\.0+)?)")) { a(Empty); saving } else
			try {
				a(Full(BigDecimal(v.replaceAll(",", "."))))
				if (a.validate.length == 0) saving
				else Fx.invalid("." + cssId, a.validate) & jsFail
			} catch { // not a number
				case _ => Fx.invalidated("." + cssId, S.??("Error"), S.??("must.be.a.number")) & jsFail
			}

		SHtml.ajaxText(a.is.toString, update(_), "class" -> css, "pattern" -> "[0-9.,]*")
	}

}


object InetUtils {
	/**
	 * Converts given String to [[java.net.InetAddress]]
	 *
	 * @param addr Valid IPv4 or IPv6 Address
	 */
	def getAddress(addr: String): java.net.InetAddress = {
		val subnetStripped = addr.replaceAll("/[0-9]+$", "")
		java.net.InetAddress.getByAddress({
			if (addr.matches(".*\\..*"))
				sun.net.util.IPAddressUtil.textToNumericFormatV4(subnetStripped)
			else
				sun.net.util.IPAddressUtil.textToNumericFormatV6(subnetStripped)
		})
	}


	/**
	 * Converts given IP Address (as String) to PTR form
	 *
	 * @param addr Valid IPv4 or IPv6 Address
	 */
	def getPtr(addr: java.net.InetAddress): String = {
		if (addr.toString().matches(".*\\..*"))
			addr.toString().split("\\.").reverse.mkString(".") + ".in-addr.arpa"
		else
			addr.toString().replaceAll(":|/[0-9]+$", "").split("").reverse.mkString(".") + ".ip6.arpa"
	}


	/**
	 * Converts given IP Address (as String) to PTR form
	 *
	 * @param addr Valid IPv4 or IPv6 Address as String
	 */
	def getPtr(addr: String): String = getPtr(getAddress(addr))

}


/**
 * Extend a snippet and use val <b>month</b> which holds a [[org.joda.time.DateTime]] of the param passed as <i>month</i> (e.g. ?month=2012-01)
 *
 * Falls back to current Month
 */
trait MonthSnippet {

	val month = S.param("month") match {
		case Full(a: String) =>
			if (a.matches("[0-9]{1,2}"))
				try {
					DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime((new DateTime).toString("yyyy-%02d-01".format(a)))
				} catch {
					case _ => new DateTime
				}
			else
				try {
					DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(a + "-01")
				} catch {
					case _ => new DateTime
				}
		case _ => new DateTime
	}
}


import java.lang.reflect._
import java.io._

/**
 * Extend a snippet and use method <b>shellExec</b> to execute a command and get the output
 *
 * Method workDir provides easy access to a Property <b>script.dir</b> to be set in Liftweb's properties-file.
 */
trait ExecSnippet {

	def workDir = Props.get("script.dir") openOr "/tmp/i.didnt.configure.jack"

	def shellExec(cmds: List[String]): List[String] = {
		val process = Runtime.getRuntime.exec(cmds.toArray)
		val resultBuffer = new BufferedReader(new InputStreamReader(process.getInputStream))
		var retList = List[String]()
		var line: String = null

		do {
			line = resultBuffer.readLine
			if (line != null) {
				retList = line :: retList
			}
		} while (line != null)

		process.waitFor
		resultBuffer.close
		retList.reverse
	}
}


