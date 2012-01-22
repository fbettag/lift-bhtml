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
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.json._
import net.liftweb.http.js._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import java.util.{Date, Calendar, TimeZone}
import java.math.{MathContext, RoundingMode}

import scala.xml._


abstract class MappedNullableInt[T <: Mapper[T]](val fieldOwner: T) extends MappedNullableField[Int, T] {
	private var data: Box[Int] = defaultValue
	private var orgData: Box[Int] = defaultValue

	def defaultValue: Box[Int] = Empty

	def dbFieldClass = classOf[Box[Int]]

	/**
	 * Get the JDBC SQL Type for this field
	 */
	def targetSQLType = Types.INTEGER

	protected def i_is_! = data

	protected def i_was_! = orgData

	/**
	 * Called after the field is saved to the database
	 */
	override def doneWithSave() {
		orgData = data
	}

	protected def real_i_set_!(value: Box[Int]): Box[Int] = {
		if (value != data) {
			data = value
			dirty_?(true)
		}
		data
	}

	def asJsExp: JsExp = is.map(v => JE.Num(v)) openOr JE.JsNull

	def asJsonValue: Box[JsonAST.JValue] =
		Full(is.map(v => JsonAST.JInt(v)) openOr JsonAST.JNull)

	override def readPermission_? = true

	override def writePermission_? = true

	def real_convertToJDBCFriendly(value: Box[Int]): Object = value match {
		case Full(value2) => new java.lang.Integer(value2)
		case _ => null
	}

	// def asJsExp = JE.Num(is)

	def jdbcFriendly(field: String) = real_convertToJDBCFriendly(i_is_!)

	override def jdbcFriendly = real_convertToJDBCFriendly(i_is_!)

	override def setFromAny(in: Any): Box[Int] = {
		in match {
			case n: Int => this.set(Full(n))
			case JsonAST.JNothing | JsonAST.JNull => this.set(Empty)
			case JsonAST.JInt(n) => this.set(Full(n.intValue))
			case (n: Number) :: _ => this.set(Full(n.intValue))
			case Some(n: Number) => this.set(Full(n.intValue))
			case Full(n: Number) => this.set(Full(n.intValue))
			case Empty | Failure(_, _, _) => this.set(Empty)
			case None => this.set(Empty)
			case (s: String) :: _ => this.set(try {
				Full(s.toInt)
			} catch {
				case _ => Empty
			})
			case s :: _ => this.setFromAny(s)
			case null => this.set(Empty)
			case s: String => this.set(try {
				Full(s.toInt)
			} catch {
				case _ => Empty
			})
			case o => this.set(Full(toInt(o)))
		}
	}

	protected def i_obscure_!(in: Box[Int]) = defaultValue

	private def st(in: Box[Int]) {
		data = in
		orgData = in
	}

	def toValue = this.is match {
		case Full(a: Int) => a.toString
		case _ => ""
	}

	def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String): (T, AnyRef) => Unit =
		(inst, v) => doField(inst, accessor, {
			case f: MappedNullableInt[T] => f.st(Full(toInt(v)))
		})

	def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
		(inst, v, isNull) => doField(inst, accessor, {
			case f: MappedNullableInt[T] => f.st(if (isNull) Empty else Full(v.toInt))
		})

	def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
		(inst, v) => doField(inst, accessor, {
			case f: MappedNullableInt[T] => f.st(Full(toInt(v)))
		})

	def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
		(inst, v) => doField(inst, accessor, {
			case f: MappedNullableInt[T] => f.st(if (v == null) Empty else Full(v.getTime.toInt))
		})

	def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit = null

	/**
	 * Given the driver type, return the string required to create the column in the database
	 */
	def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longColumnType + notNullAppender()
}


abstract class MappedNullableDecimal[T <: Mapper[T]](val fieldOwner: T, val context: MathContext, val scale: Int)
extends MappedNullableField[BigDecimal, T] {
	private var data: Box[BigDecimal] = defaultValue
	private var orgData: Box[BigDecimal] = defaultValue

	def defaultValue: Box[BigDecimal] = Empty

	def dbFieldClass = classOf[Box[BigDecimal]]

	/**
	 * Get the JDBC SQL Type for this field
	 */
	def targetSQLType = Types.DECIMAL

	protected def i_is_! = data

	protected def i_was_! = orgData

	/**
	 * Called after the field is saved to the database
	 */
	override def doneWithSave() {
		orgData = data
	}

	protected def real_i_set_!(value: Box[BigDecimal]): Box[BigDecimal] = {
		if (value != data) {
			data = value
			dirty_?(true)
		}
		data
	}

	def asJsExp: JsExp = is.map(v => JE.Num(v.doubleValue())) openOr JE.JsNull

	def asJsonValue: Box[JsonAST.JValue] = Full(is.map(v => JsonAST.JDouble(v.doubleValue())) openOr JsonAST.JNull)

	override def readPermission_? = true

	override def writePermission_? = true

	def real_convertToJDBCFriendly(value: Box[BigDecimal]): Object = value match {
		case Full(value2) => coerce(value2)
		case _ => null
	}

	// Set the scale on the given input
	protected def coerce(in: BigDecimal) = new BigDecimal(in.bigDecimal.setScale(scale, context.getRoundingMode))

	def jdbcFriendly(field: String) = real_convertToJDBCFriendly(i_is_!)

	override def jdbcFriendly = real_convertToJDBCFriendly(i_is_!)

	override def setFromAny(in: Any): Box[BigDecimal] = {
		in match {
			case bd: BigDecimal => setAll(bd)
			case n :: _ => setFromString(n.toString)
			case Some(n) => setFromString(n.toString)
			case Full(n) => setFromString(n.toString)
			case None | Empty | Failure(_, _, _) | null => this.set(Empty)
			case n => setFromString(n.toString)
		}
	}

	def setFromString(in: String): Box[BigDecimal] = {
		try {
			this.setAll(BigDecimal(in))
		} catch {
			case _ =>
		}
		data
	}

	protected def setAll(in: BigDecimal) = this.set(Full(coerce(in)))

	protected def i_obscure_!(in: Box[BigDecimal]) = defaultValue

	private def st(in: Box[BigDecimal]) {
		data = in
		orgData = in
	}

	def toValue = this.is match {
		case Full(a: BigDecimal) => a.toString
		case _ => ""
	}

	def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String): (T, AnyRef) => Unit =
		(inst, v) => doField(inst, accessor, {
			case f: MappedNullableDecimal[T] => f.st(if (v == null) Empty else Full(coerce(BigDecimal(v.toString))))
		})

	def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
		(inst, v, isNull) => doField(inst, accessor, {
			case f: MappedNullableDecimal[T] => f.st(if (isNull) Empty else Full(coerce(BigDecimal(v))))
		})

	def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
		(inst, v) => doField(inst, accessor, {
			case f: MappedNullableDecimal[T] => f.st(if (v == null) Empty else Full(coerce(BigDecimal(v))))
		})

	def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
		(inst, v) => doField(inst, accessor, {
			case f: MappedNullableDecimal[T] => f.st(if (v == null) Empty else Full(coerce(BigDecimal(v.getTime))))
		})

	def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit = null

	/**
	 * Given the driver type, return the string required to create the column in the database
	 */
	def fieldCreatorString(dbType: DriverType, colName: String): String = {
		val suffix = if (context.getPrecision == 0) {
			""
		} else {
			"(" + context.getPrecision + "," + scale + ")"
		}

		colName + " DECIMAL" + suffix + notNullAppender()
	}
}

