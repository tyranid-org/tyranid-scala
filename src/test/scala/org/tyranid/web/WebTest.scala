/*
 * Copyright (c) 2008-2012 Tyranid <http://tyranid.org>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.tyranid.web

import java.util.{ Calendar, Date, TimeZone }

import scala.xml.Unparsed

import org.scalatest.FunSuite

import org.tyranid.Imp._
import org.tyranid.session.Session


class WebSuite extends FunSuite {
  org.tyranid.boot.Boot.boot

  test( "templates" ) {

    assert(
      WebTemplate(
        <tyr:shell><div><p>Alpha</p></div></tyr:shell> ).toString ===
      <html><body><h1>hi there</h1><div><p>Alpha</p></div></body></html>.toString )

    assert(
      WebTemplate(
        <tyr:shell><tyr:sample/></tyr:shell> ).toString ===
      <html><body><h1>hi there</h1><p>Sample</p></body></html>.toString )

    assert(
      WebTemplate(
        <div><p>Beta</p><tyr:content/></div>,
        <input/> ).toString ===
      <div><p>Beta</p><input></input></div>.toString )

    assert(
      WebTemplate(
        <html><head><s>foo</s></head><d><p>Gamma</p><tyr:content/></d></html>,
        <p><head><s>bar</s></head><i/></p> ).toString ===
      <html><head><s>foo</s><s>bar</s></head><d><p>Gamma</p><p><i></i></p></d></html>.toString )

    assert(
      WebTemplate(
        Unparsed( """<!DOCTYPE html>""" ) ++ <html xmlns="https://www.w3.org/1999/xhtml"><head><script>foo</script></head><d><p>Theta</p><tyr:content/></d></html>,
        <p><head><s>bar</s></head><head><script>{ Unparsed( "var js = 'foo';" ) }</script></head><i/></p> ).toString ===
      ( Unparsed( """<!DOCTYPE html>""" ) ++ <html xmlns="https://www.w3.org/1999/xhtml"><head><script>foo</script><s>bar</s><script>{ Unparsed( "var js = 'foo';" ) }</script></head><d><p>Theta</p><p><i></i></p></d></html> ).toString )

    assert(
      WebTemplate(
        <html><body><tail><p>hi</p></tail><div><p>Delta</p></div></body></html> ).toString ===
      <html><body><div><p>Delta</p></div><p>hi</p></body></html>.toString )

    assert(
      WebTemplate(
        <tyr:shell><tail><div>foo</div></tail><div><p>Omega</p></div></tyr:shell> ).toString ===
      <html><body><h1>hi there</h1><div><p>Omega</p></div><div>foo</div></body></html>.toString )

    assert(
      WebTemplate(
        <tyr:shell><tail><div>foo</div></tail><div><p>Epsilon</p></div><top><p>top</p></top></tyr:shell> ).toString ===
      <html><body><p>top</p><h1>hi there</h1><div><p>Epsilon</p></div><div>foo</div></body></html>.toString )
  }
}

