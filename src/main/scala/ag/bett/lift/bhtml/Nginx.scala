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

import net.liftweb.http._


/**
 * @param prefix Prefix nginx is configured to serve [[http://wiki.nginx.org/HttpCoreModule#internal internal requests]]
 * @param fileName Guess what..
 * @param mimeType Big mystery as well..
 * @param headers This seems increasingly harder :/
 *
 * @return LiftResponse or more specific InMemoryResponse with [[http://wiki.nginx.org/X-accel nginx X-accel]] headers
 */
case class NginxSendfileResponse(sendfilePrefix: String, fileName: String, mimeType: String, headers: List[(String, String)]) extends LiftResponse {
	val allHeaders:List[(String,String)] =
		("Content-Type" -> mimeType) ::
		("X-Content-Type-Options" -> "nosniff") ::
		("X-Accel-Buffering" -> "yes") ::
		("X-Accel-Redirect" -> "%s/%s".format(sendfilePrefix, fileName)) ::
		headers

	def toResponse = new InMemoryResponse(Array(), allHeaders, Nil, 200)
	override def toString = "NginxSendfileResponse(data, " + mimeType + ", " + fileName + ", " + headers + ")"

}



