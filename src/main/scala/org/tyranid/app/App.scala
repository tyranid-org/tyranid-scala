package org.tyranid.app

import java.util.Date

import com.mongodb.DBObject

import org.tyranid.Imp._

import org.tyranid.db.{ DbInt, DbChar, DbDateTime, DbLink }
import org.tyranid.db.mongo._
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.tuple.{ Tuple, TupleView }
import org.tyranid.db.ram.RamEntity

object AppStatType extends RamEntity( tid = "a1Fv" ) {
  type RecType = AppStatType
  override def convert( view:TupleView ) = new AppStatType( view )

  "_id"     is DbInt      is 'id;
  "name"    is DbChar(64) is 'label;

  override val addNames = Seq( "_id", "name" )
  
  val CrocodocUploadId       = 100
  val CrocodocRetryId        = 101
  val CrocodocSuccessId      = 102
  val CrocodocFailureId      = 103
  
  val PdfCrowdSendId         = 200
  val PdfCrowdRetryId        = 201
  val PdfCrowdSuccessId      = 202
  val PdfCrowdFailureId      = 203
  
  val PdfConvertApiSendId    = 300
  val PdfConvertApiRetryId   = 301
  val PdfConvertApiSuccessId = 302
  val PdfConvertApiFailureId = 303
  
  val ZencoderUploadId       = 400
  val ZencoderSuccessId      = 401
  val ZencoderFailureId      = 402
  
  val CrocodocUpload  = add( CrocodocUploadId,  "Crocodoc Upload" )
  val CrocodocRetry   = add( CrocodocRetryId,   "Crocodoc Retry" )
  val CrocodocSuccess = add( CrocodocSuccessId, "Crocodoc Success" )
  val CrocodocFailure = add( CrocodocFailureId, "Crocodoc Failure" )
  
  val PdfCrowdSend    = add( PdfCrowdSendId,    "PdfCrowd Send" )
  val PdfCrowdRetry   = add( PdfCrowdRetryId,   "PdfCrowd Retry" )
  val PdfCrowdSuccess = add( PdfCrowdSuccessId, "PdfCrowd Success" )
  val PdfCrowdFailure = add( PdfCrowdFailureId, "PdfCrowd Failure" )
  
  val PdfConvertApiSend    = add( PdfConvertApiSendId,    "PdfConvertApi Send" )
  val PdfConvertApiRetry   = add( PdfConvertApiRetryId,   "PdfConvertApi Retry" )
  val PdfConvertApiSuccess = add( PdfConvertApiSuccessId, "PdfConvertApi Success" )
  val PdfConvertApiFailure = add( PdfConvertApiFailureId, "PdfConvertApi Failure" )
  
  val ZencoderUpload  = add( ZencoderUploadId,  "Zencoder Upload" )
  val ZencoderSuccess = add( ZencoderSuccessId, "Zencoder Success" )
  val ZencoderFailure = add( ZencoderFailureId, "Zencoder Failure" )  
}

case class AppStatType( override val view:TupleView ) extends Tuple( view )

object AppStat extends MongoEntity( tid = "b04v" ) {
  "_id"  is DbMongoId            is 'id is 'client;
  "s"    is DbLink(AppStatType)  is 'required;
  "t"    is DbDateTime           is 'required;
  
  val index = {
    db.ensureIndex( Mobj( "s" -> 1, "u" -> 1, "t" -> 1 ) )
    db.ensureIndex( Mobj( "u" -> 1, "s" -> 1, "t" -> 1 ) )
  }
  
  private def create( statId:Int ) {
    background {
      val stat = AppStat.make
      stat( 's ) = statId
      stat( 't ) = new Date
      
      stat.save
    }
  }
  
  def CrocodocUpload  = create( AppStatType.CrocodocUploadId )
  def CrocodocRetry   = create( AppStatType.CrocodocRetryId )
  def CrocodocSuccess = create( AppStatType.CrocodocSuccessId )
  def CrocodocFailure = create( AppStatType.CrocodocFailureId )
  
  def PdfCrowdSend    = create( AppStatType.PdfCrowdSendId )
  def PdfCrowdRetry   = create( AppStatType.PdfCrowdRetryId )
  def PdfCrowdSuccess = create( AppStatType.PdfCrowdSuccessId )
  def PdfCrowdFailure = create( AppStatType.PdfCrowdFailureId )

  def PdfConvertApiSend    = create( AppStatType.PdfConvertApiSendId )
  def PdfConvertApiRetry   = create( AppStatType.PdfConvertApiRetryId )
  def PdfConvertApiSuccess = create( AppStatType.PdfConvertApiSuccessId )
  def PdfConvertApiFailure = create( AppStatType.PdfConvertApiFailureId )
  
  def ZencoderUpload  = create( AppStatType.ZencoderUploadId )
  def ZencoderSuccess = create( AppStatType.ZencoderSuccessId )
  def ZencoderFailure = create( AppStatType.ZencoderFailureId )
}

class AppStat( obj:DBObject, parent:MongoRecord ) extends MongoRecord( AppStat.makeView, obj, parent ) {}
