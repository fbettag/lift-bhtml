/* {{{
* Copyright (c) 2011, Franz Bettag <franz@bett.ag>
*   All rights reserved.
*
*   Redistribution and use in source and binary forms, with or without
*   modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*
*   THIS SOFTWARE IS PROVIDED BY BETTAG SYSTEMS UG ''AS IS'' AND ANY
*   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*   DISCLAIMED. IN NO EVENT SHALL BETTAG SYSTEMS UG BE LIABLE FOR ANY
*   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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


object BHtml {

	def delay(d: Int, js: JsCmd) = JsRaw("""window.setTimeout(function() {%s}, %s);""".format(js.toJsCmd, d))

	def hide(sel: String): JsCmd = JsRaw("""$('%s').hide()""".format(sel))
	def show(sel: String): JsCmd = JsRaw("""$('%s').show()""".format(sel))
	def toggle(sel: String): JsCmd = JsRaw("""$('%s').toggle()""".format(sel))

	object Fx {
		def success(sel: String): JsCmd =
			JsRaw("""$('%s').effect('pulsate', {times: 2}, 200)""".format(sel)).cmd

		def failed(sel: String): JsCmd =
			JsRaw("""$('%s').effect('pulsate', {times: 2}, 200)""".format(sel)).cmd

		def invalid(sel: String, errors: List[FieldError]): JsCmd =
			invalidated(sel, S.??("Error"), <ul>{errors.map(fe => <li>{fe.msg}</li>)}</ul>)

		def validated(sel: String): JsCmd =
			JsRaw("""$('%s').removeClass('error').addClass('success').removeAttr('title data-original-title data-content')""".format(sel)).cmd

		def invalidated(sel: String): JsCmd =
			JsRaw("""$('%s').removeClass('success').addClass('error')""".format(sel)).cmd

		def invalidated(sel: String, title: String, body: String): JsCmd =
			invalidated(sel, title, Text(body))

		def invalidated(sel: String, title: String, body: NodeSeq): JsCmd =
			invalidated(sel) & popover(sel, title, body) & delay(2000, JsRaw("""$('%s').popover('hide')""".format(sel)).cmd)

		def reset(sel: String): JsCmd =
			JsRaw("""$('%s').removeClass('error').removeClass('success').removeAttr('title data-original-title data-content')""".format(sel)).cmd

		def remove(sel: String): JsCmd =
			JsRaw("""$('%s').fadeOut(400).hide(function() { $('%s').remove(); })""".format(sel, sel)).cmd

		def popover(sel: String, title: String, body: NodeSeq): JsCmd =
			JsRaw("""$('%s').attr('title', "%s").attr('data-content', "%s").popover({ offset: 10, html: true }).popover('show').click(function(e) { e.preventDefault() })"""
				.format(sel, title.replaceAll("\"", "\\\""), body.toString.replaceAll("\"", "\\\""))).cmd

	}


	def getCssId[K, T <: KeyedMapper[K, T]](a: MappedField[K, T]) = "BF-%s-%s".format(a.uniqueFieldId.open_!, a.fieldOwner.primaryKeyField.is)


	/**
	 * Save a Record
	 */
	def save[K, T <: KeyedMapper[K, T]](a: T, jsSuccess: () => JsCmd = () => Noop, jsFail: () => JsCmd = () => Noop): JsCmd = {
		if (a.validate.length == 0 && a.save) jsSuccess()
		else jsFail() & a.allFields.map(bf => {
			if (bf.validate.length == 0) Noop
			else Fx.invalid(".BF-%s-%s".format(bf.uniqueFieldId.open_!, a.primaryKeyField.is), bf.validate)
		})
	}

	/**
	 * Reset validations for a Record
	 */
	def resetValidation[K, T <: KeyedMapper[K, T]](a: T): JsCmd =
		a.allFields.map(bf => Fx.reset(".BF-%s-%s".format(bf.uniqueFieldId.open_!, a.primaryKeyField.is)))


	/**
	 * Mapped Boolean
	 */
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
	

	/**
	 * Mapped String
	 */
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


	/**
	 * Mapped Int
	 */
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

	/**
	 * Mapped Nullable Int
	 */
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


	/**
	 * Mapped Decimal
	 */
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

	/**
	 * Mapped Nullable Int
	 */
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



