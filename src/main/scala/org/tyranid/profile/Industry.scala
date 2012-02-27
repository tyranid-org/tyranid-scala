/**
 * Copyright (c) 2008-2011 Tyranid (   http://tyranid.org>
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

package org.tyranid.profile

import scala.collection.mutable

import org.bson.types.ObjectId
import com.mongodb.DBObject

import org.tyranid.Imp._
import org.tyranid.db.{ DbChar, DbInt, DbLong, DbUrl }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity }
import org.tyranid.db.meta.AutoIncrement
import org.tyranid.db.ram.RamEntity


object Industry extends MongoEntity( tid = "a0O0" ) {
	override lazy val dbName = "businessCategories"

  "sectorId"        is DbInt               ;
  "sectorName"      is DbChar(30)          ;
  "industryGroupId" is DbInt               ;
  "industryGroup"   is DbChar(30)          ;
  "industryId"      is DbInt               ;
  "industry"        is DbChar(40)          ;
  "subIndustryId"   is DbLong              ;//is Req( UserRole.Admin ); // subIndustryId
  "subIndustry"     is DbChar(50) is 'label;
  "description"     is DbChar(400)         ;

  def cache = mutable.HashMap[ObjectId,DBObject]()

  def get( id:ObjectId ) = cache.getOrElseUpdate( id, Industry.db.findOne( id ) )

  //temporary
  def old2new( rec:org.tyranid.db.Record, name:String ) {
    var a = rec.a( name )

    if ( a == null ) {
      a = new com.mongodb.BasicDBList
      rec( name ) = a
    }

    var updates = false

    for ( i <- 0 until a.size ) {
      a( i ) match {
      case oid:ObjectId =>

        val old = Industry.db.findOne( Mobj( "_id" -> oid ) )
        a( i ) = old.i( 'subIndustryId ) + 100000000
        updates = true

      case _ =>
      }
    }

    if ( updates )
      rec.save
  }

  def old2new( rec:org.tyranid.db.Record ) {
    old2new( rec, "buyingCategories" )
    old2new( rec, "sellingCategories" )
  }
}

object IndustryType extends RamEntity( tid = "a0Ou" ) {
  "id"              is DbInt              is 'key;
  "name"            is DbChar(30)         ;
  "source"          is DbUrl              ;

  static(
  ( "id", "name",                                           "source" ),
  (    1, "GICS (Global Industry Classification Standard)", "http://www.standardandpoors.com/indices/gics/en/us" ),
  (    2, "LinkedIn",                                       "https://developer.linkedin.com/documents/industry-codes" ) )
}

object GicsSector extends RamEntity( tid = "a0O1" ) {
  "id"              is DbInt              is 'key;
  "name"            is DbChar(30)         ;

  static(
  ( "id", "name" ),
  (   10, "Energy" ),
  (   15, "Materials" ),
  (   20, "Industrials" ),
  (   25, "Consumer Discretionary" ),
  (   30, "Consumer Staples" ),
  (   35, "Health Care" ),
  (   40, "Financials" ),
  (   45, "Information Technology" ),
  (   50, "Telecommunication Services" ),
  (   55, "Utilities" ) )
}

object GicsIndustryGroup extends RamEntity( tid = "a0O2" ) {
  "id"              is DbInt               is 'key;
  "name"            is DbChar(30)          ;

  static(
  ( "id", "name" ),
  ( 1010, "Energy" ),
  ( 1510, "Materials" ),
  ( 2010, "Capital Goods" ),
  ( 2020, "Commercial & Professional Services" ),
  ( 2030, "Transportation" ),
  ( 2510, "Automobiles & Components" ),
  ( 2520, "Consumer Durables & Apparel" ),
  ( 2530, "Consumer Services" ),
  ( 2540, "Media" ),
  ( 2550, "Retailing" ),
  ( 3010, "Food & Staples Retailing" ),
  ( 3020, "Food, Beverage & Tobacco" ),
  ( 3030, "Household & Personal Products" ),
  ( 3510, "Health Care Equipment & Services" ),
  ( 3520, "Pharmaceuticals, Biotechnology & Life Sciences" ),
  ( 4010, "Banks" ),
  ( 4020, "Diversified Financials" ),
  ( 4030, "Insurance" ),
  ( 4040, "Real Estate" ),
  ( 4510, "Software & Services" ),
  ( 4520, "Technology Hardware & Equipment" ),
  ( 4530, "Semiconductors & Semiconductor Equipment" ),
  ( 5010, "Telecommunication Services" ),
  ( 5510, "Utilities" ) )
}

object GicsIndustry extends RamEntity( tid = "a0O3" ) {
  "id"              is DbInt              is 'key;
  "name"            is DbChar(40)         ;

  static(
  ( "id",   "name" ),
  ( 101010, "Energy Equipment & Services" ),
  ( 101020, "Oil, Gas & Consumable Fuels" ),
  ( 151010, "Chemicals" ),
  ( 151020, "Construction Materials" ),
  ( 151030, "Containers & Packaging" ),
  ( 151040, "Metals & Mining" ),
  ( 151050, "Paper & Forest Products" ),
  ( 201010, "Aerospace & Defense" ),
  ( 201020, "Building Products" ),
  ( 201030, "Construction & Engineering" ),
  ( 201040, "Electrical Equipment" ),
  ( 201050, "Industrial Conglomerates" ),
  ( 201060, "Machinery" ),
  ( 201070, "Trading Companies & Distributors" ),
  ( 202010, "Commercial Services & Supplies" ),
  ( 202020, "Professional Services" ),
  ( 203010, "Air Freight & Logistics" ),
  ( 203020, "Airlines" ),
  ( 203030, "Marine" ),
  ( 203040, "Road & Rail" ),
  ( 203050, "Transportation Infrastructure" ),
  ( 251010, "Auto Components" ),
  ( 251020, "Automobiles" ),
  ( 252010, "Household Durables" ),
  ( 252020, "Leisure Equipment & Products" ),
  ( 252030, "Textiles, Apparel & Luxury Goods" ),
  ( 253010, "Hotels, Restaurants & Leisure" ),
  ( 253020, "Diversified Consumer Services" ),
  ( 254010, "Media" ),
  ( 255010, "Distributors" ),
  ( 255020, "Internet & Catalog Retail" ),
  ( 255030, "Multiline Retail" ),
  ( 255040, "Specialty Retail" ),
  ( 301010, "Food & Staples Retailing" ),
  ( 302010, "Beverages" ),
  ( 302020, "Food Products" ),
  ( 302030, "Tobacco" ),
  ( 303010, "Household Products" ),
  ( 303020, "Personal Products" ),
  ( 351010, "Health Care Equipment & Supplies" ),
  ( 351020, "Health Care Providers & Services" ),
  ( 351030, "Health Care Technology" ),
  ( 352010, "Biotechnology" ),
  ( 352020, "Pharmaceuticals" ),
  ( 352030, "Life Sciences Tools & Services" ),
  ( 401010, "Commercial Banks" ),
  ( 401020, "Thrifts & Mortgage Finance" ),
  ( 402010, "Diversified Financial Services" ),
  ( 402020, "Consumer Finance" ),
  ( 402030, "Capital Markets" ),
  ( 403010, "Insurance" ),
  ( 404010, "Real Estate" ),
  ( 404020, "Real Estate Investment Trusts (REITs)" ),
  ( 404030, "Real Estate Management & Development" ),
  ( 451010, "Internet Software & Services" ),
  ( 451020, "IT Services" ),
  ( 451030, "Software" ),
  ( 452010, "Communications Equipment" ),
  ( 452020, "Computers & Peripherals" ),
  ( 452030, "Electronic Equipment, Instruments & Components" ),
  ( 452040, "Office Electronics" ),
  ( 452050, "Semiconductor Equipment & Products" ),
  ( 453010, "Semiconductors & Semiconductor Equipment" ),
  ( 501010, "Diversified Telecommunication Services" ),
  ( 501020, "Wireless Telecommunication Services" ),
  ( 551010, "Electric Utilities" ),
  ( 551020, "Gas Utilities" ),
  ( 551030, "Multi-Utilities" ),
  ( 551040, "Water Utilities" ),
  ( 551050, "Independent Power Producers & Energy Traders" ) )
}

object LinkedInCategory extends RamEntity( tid = "a0O4" ) {
  override lazy val dbName = "businessCategories"

  "id"     is DbChar(8) ;
  "name"   is DbChar(50);

  static(
  ( "id",   "name" ),
  ( "agr",  "Agriculture" ),
  ( "art",  "Arts" ),
  ( "cons", "Construction" ),
  ( "corp", "Corporate" ),
  ( "edu",  "Education" ),
  ( "fin",  "Finance" ),
  ( "good", "Goods" ),
  ( "gov",  "Government" ),
  ( "hlth", "Healthcare" ),
  ( "leg",  "Legal" ),
  ( "man",  "Manufacturing" ),
  ( "med",  "Media" ),
  ( "org",  "Organization" ),
  ( "rec",  "Recreation" ),
  ( "serv", "Services" ),
  ( "tech", "Technology" ),
  ( "tran", "Transportation" ) )
}

object NewIndustry extends RamEntity( tid = "a0O5" ) {
	override lazy val dbName = "businessCategories"

  "id"             is DbInt              is 'key;
  "category"       is DbChar(64)         ;
  "name"           is DbChar(64)         ;
  "description"    is DbChar(1024)       ;

  // for gics, name is "subIndustry"

  static(
( "id",     "category",        "name",                                           "description" ),
( 110101010, null,             "Oil & Gas Drilling",                             "Drilling contractors or owners of drilling rigs that contract their services for drilling wells" ),
( 110101020, null,             "Oil & Gas Equipment & Services",                 "Manufacturers of equipment, including drilling rigs and equipment, and providers of supplies and services to companies involved in the drilling, evaluation and completion of oil and gas wells." ),
( 110102010, null,             "Integrated Oil & Gas",                           "Integrated oil companies engaged in the exploration & production of oil and gas, as well as at least one other significant activity in either refining, marketing and transportation, or chemicals." ),
( 110102020, null,             "Oil & Gas Exploration & Production",             "Companies engaged in the exploration and production of oil and gas not classified elsewhere." ),
( 110102030, null,             "Oil & Gas Refining & Marketing",                 "Companies engaged in the refining and marketing of oil, gas and/or refined products not classified in the Integrated Oil & Gas or Independent Power Producers & Energy Traders Sub-Industries." ),
( 110102040, null,             "Oil & Gas Storage & Transportation",             "Companies engaged in the storage and/or transportation of oil, gas and/or refined products. Includes diversified midstream natural gas companies facing competitive markets, oil and refined product pipelines, coal slurry pipelines and oil & gas shipping companies." ),
( 110102050, null,             "Coal & Consumable Fuels",                        "Companies primarily involved in the production and mining of coal, related products and other consumable fuels related to the generation of energy.  Excludes companies primarily producing gases classified in the Industrial Gases sub-industry and companies primarily mining for metallurgical (coking) coal used for steel production." ),
( 115101010, null,             "Commodity Chemicals",                            "Companies that primarily produce industrial chemicals and basic chemicals. Including but not limited to plastics, synthetic fibers, films, commodity-based paints & pigments, explosives and petrochemicals. Excludes chemical companies classified in the Diversified Chemicals, Fertilizers & Agricultural Chemicals, Industrial Gases, or Specialty Chemicals Sub-Industries." ),
( 115101020, null,             "Diversified Chemicals",                          "Manufacturers of a diversified range of chemical products not classified in the Industrial Gases, Commodity Chemicals, Specialty Chemicals or Fertilizers & Agricultural Chemicals Sub-Industries." ),
( 115101030, null,             "Fertilizers & Agricultural Chemicals",           "Producers of fertilizers, pesticides, potash or other agriculture-related chemicals not classified elsewhere." ),
( 115101040, null,             "Industrial Gases",                               "Manufacturers of industrial gases." ),
( 115101050, null,             "Specialty Chemicals",                            "Companies that primarily produce high value-added chemicals used in the manufacture of a wide variety of products, including but not limited to fine chemicals, additives, advanced polymers, adhesives, sealants and specialty paints, pigments and coatings." ),
( 115102010, null,             "Construction Materials",                         "Manufacturers of construction materials including sand, clay, gypsum, lime, aggregates, cement, concrete and bricks. Other finished or semi-finished building materials are classified  in the Building Products Sub-Industry." ),
( 115103010, null,             "Metal & Glass Containers",                       "Manufacturers of metal, glass or plastic containers. Includes corks and caps." ),
( 115103020, null,             "Paper Packaging",                                "Manufacturers of paper and cardboard containers and packaging." ),
( 115104010, null,             "Aluminum",                                       "Producers of aluminum and related products, including companies that mine or process bauxite and companies that recycle aluminum to produce finished or semi-finished products. Excludes companies that primarily produce aluminum building materials classified in the Building Products Sub-Industry." ),
( 115104020, null,             "Diversified Metals & Mining",                    "Companies engaged in the diversified production or extraction of metals and minerals not classified elsewhere. Including, but not limited to, nonferrous metal mining (except aluminum), salt and borate mining, phosphate rock mining, metallurgical (coking) coal mining used for steel production and diversified mining operations. Excludes iron ore mining, classified in the Steel Sub-Industry and aluminum mining classified in the Aluminum Sub-Industry.  Excludes bituminous (thermal) coal-mining companies classified in the Coal & Consumable Fuels Sub-Industry." ),
( 115104030, null,             "Gold",                                           "Producers of gold and related products, including companies that mine or process gold and the South African finance houses which primarily invest in, but do not operate, gold mines." ),
( 115104040, null,             "Precious Metals & Minerals",                     "Companies mining precious metals and minerals not classified in the Gold Sub-Industry. Includes companies primarily mining platinum." ),
( 115104050, null,             "Steel",                                          "Producers of iron and steel and related products" ),
( 115105010, null,             "Forest Products",                                "Manufacturers of timber and related wood products. Includes lumber for the building industry." ),
( 115105020, null,             "Paper Products",                                 "Manufacturers of all grades of paper. Excludes companies specializing in paper packaging classified in the Paper Packaging Sub-Industry." ),
( 120101010, null,             "Aerospace & Defense",                            "Manufacturers of civil or military aerospace and defense equipment, parts or products. Includes defense electronics and space equipment." ),
( 120102010, null,             "Building Products",                              "Manufacturers of building components and home improvement products and equipment. Excludes lumber and plywood classified under Forest Products and cement and other materials classified in the Construction Materials Sub-Industry." ),
( 120103010, null,             "Construction & Engineering",                     "Companies engaged in primarily non-residential construction. Includes civil engineering companies and large-scale contractors. Excludes companies classified in the Homebuilding Sub-Industry." ),
( 120104010, null,             "Electrical Components & Equipment",              "Companies that produce electric cables and wires, electrical components or equipment not classified in the Heavy Electrical Equipment Sub-Industry." ),
( 120104020, null,             "Heavy Electrical Equipment",                     "Manufacturers of power-generating equipment and other heavy electrical equipment, including power turbines, heavy electrical machinery intended for fixed-use and large electrical systems. Excludes cables and wires, classified in the Electrical Components & Equipment Sub-Industry." ),
( 120105010, null,             "Industrial Conglomerates",                       "Diversified industrial companies with business activities in three or more sectors, none of which contributes a majority of revenues. Stakes held are predominantly of a controlling nature and stake holders maintain an operational interest in the running of the subsidiaries." ),
( 120106010, null,             "Construction & Farm Machinery & Heavy Trucks",   "Manufacturers of heavy duty trucks, rolling machinery, earth-moving and construction equipment, heavy farm machinery and manufacturers of related parts. Includes non-military shipbuilding." ),
( 120106020, null,             "Industrial Machinery",                           "Manufacturers of industrial machinery and industrial components. Includes companies that manufacture presses, machine tools, compressors, pollution control equipment, elevators, escalators, insulators, pumps, roller bearings and other metal fabrications." ),
( 120107010, null,             "Trading Companies & Distributors",               "Trading companies and other distributors of industrial equipment and products." ),
( 120201010, null,             "Commercial Printing",                            "Companies providing commercial printing services. Includes printers primarily serving the media industry." ),
( 120201020, null,             "Data Processing Services",                       "EXP 04/30/2003 -- Providers of commercial electronic data processing services." ),
( 120201030, null,             "Diversified Commercial & Professional Services", "EXP 08/31/2008 -- Companies primarily providing commercial, industrial and professional services to businesses and governments not classified elsewhere.  Includes commercial cleaning services, consulting services, correctional facilities, dining & catering services, document & communication services, equipment repair services, security & alarm services, storage & warehousing, and uniform rental services." ),
( 120201040, null,             "Human Resource & Employment Services",           "EXP 08/31/2008 -- Companies providing business support services relating to human capital management. Includes employment agencies, employee training, payroll & benefit support services, retirement support services and temporary agencies." ),
( 120201050, null,             "Environmental & Facilities Services",            "Companies providing environmental and facilities maintenance services. Includes waste management, facilities management and pollution control services.  Excludes large-scale water treatment systems classified in the Water Utilities Sub-Industry." ),
( 120201060, null,             "Office Services & Supplies",                     "Providers of office services and manufacturers of office supplies and equipment not classified elsewhere." ),
( 120201070, null,             "Diversified Support Services",                   "Companies primarily providing labor oriented support services to businesses and governments.  Includes commercial cleaning services, dining & catering services, equipment repair services, industrial maintenance services, industrial auctioneers, storage & warehousing, transaction services, uniform rental services, and other business support services." ),
( 120201080, null,             "Security & Alarm Services",                      "Companies providing security and protection services to business and governments. Includes companies providing services such as correctional facilities, security & alarm services, armored transportation & guarding.  Excludes companies providing security software classified under the Systems Software Sub-Industry and home security services classified under the Specialized Consumer Services Sub-Industry. Also excludes companies manufacturing security system equipment classified under the Electronic Equipment & Instruments Sub-Industry." ),
( 120202010, null,             "Human Resource & Employment Services",           "Companies providing business support services relating to human capital management. Includes employment agencies, employee training, payroll & benefit support services, retirement support services and temporary agencies." ),
( 120202020, null,             "Research & Consulting Services",                 "Companies primarily providing research and consulting services to businesses and governments not classified elsewhere.  Includes companies involved in management consulting services, architectural design, business information or scientific research, marketing, and testing & certification services. Excludes companies providing information technology consulting services classified in the IT Consulting & Other Services Sub-Industry." ),
( 120301010, null,             "Air Freight & Logistics",                        "Companies providing air freight transportation, courier and logistics services, including package and mail delivery and customs agents. Excludes those companies classified in the Airlines, Marine or Trucking Sub-Industries." ),
( 120302010, null,             "Airlines",                                       "Companies providing primarily passenger air transportation." ),
( 120303010, null,             "Marine",                                         "Companies providing goods or passenger maritime transportation. Excludes cruise-ships classified in the Hotels, Resorts & Cruise Lines Sub-Industry." ),
( 120304010, null,             "Railroads",                                      "Companies providing primarily goods and passenger rail  transportation." ),
( 120304020, null,             "Trucking",                                       "Companies providing primarily goods and passenger land transportation. Includes vehicle rental and taxi companies." ),
( 120305010, null,             "Airport Services",                               "Operators of airports and companies providing related services." ),
( 120305020, null,             "Highways & Railtracks",                          "Owners and operators of roads, tunnels and railtracks." ),
( 120305030, null,             "Marine Ports & Services",                        "Owners and operators of marine ports and related services." ),
( 125101010, null,             "Auto Parts & Equipment",                         "Manufacturers of parts and accessories for  automobiles and motorcycles. Excludes companies classified in the Tires & Rubber Sub-Industry." ),
( 125101020, null,             "Tires & Rubber",                                 "Manufacturers of tires and rubber." ),
( 125102010, null,             "Automobile Manufacturers",                       "Companies that produce mainly passenger automobiles and light trucks. Excludes companies producing mainly motorcycles and three-wheelers classified in the Motorcycle Manufacturers Sub-Industry and heavy duty trucks classified in the Construction & Farm Machinery & Heavy Trucks Sub-Industry." ),
( 125102020, null,             "Motorcycle Manufacturers",                       "Companies that produce motorcycles, scooters or three-wheelers. Excludes bicycles classified in the Leisure Products Sub-Industry." ),
( 125201010, null,             "Consumer Electronics",                           "Manufacturers of consumer electronics products including TVs, VCRs, hi-fi equipment, game consoles and related products. Excludes personal home computer manufacturers classified in the Computer Hardware Sub-Industry, and electric household appliances classified in the Household Appliances Sub-Industry." ),
( 125201020, null,             "Home Furnishings",                               "Manufacturers of soft home furnishings or furniture, including upholstery, carpets and wall-coverings." ),
( 125201030, null,             "Homebuilding",                                   "Residential construction companies. Includes manufacturers of prefabricated houses and semi-fixed manufactured homes." ),
( 125201040, null,             "Household Appliances",                           "Manufacturers of electric household appliances and related products.  Includes manufacturers of power and hand tools, including garden improvement tools.  Excludes TVs and other audio and video products classified in the Consumer Electronics Sub-Industry and personal computers classified in the Computer Hardware Sub-Industry." ),
( 125201050, null,             "Housewares & Specialties",                       "Manufacturers of durable household products, including cutlery, cookware, glassware, crystal, silverware, utensils, kitchenware and consumer specialties not classified elsewhere." ),
( 125202010, null,             "Leisure Products",                               "Manufacturers of leisure products and equipment including sports equipment, bicycles and toys." ),
( 125202020, null,             "Photographic Products",                          "Manufacturers of photographic equipment and related products." ),
( 125203010, null,             "Apparel, Accessories & Luxury Goods",            "Manufacturers of apparel, accessories & luxury goods. Includes companies primarily producing designer handbags, wallets, luggage, jewelry and watches. Excludes shoes classified in the Footwear Sub-Industry." ),
( 125203020, null,             "Footwear",                                       "Manufacturers of footwear. Includes sport and leather shoes." ),
( 125203030, null,             "Textiles",                                       "Manufacturers of textile and related products not classified in the Apparel, Accessories & Luxury Goods, Footwear or Home Furnishings Sub-Industries." ),
( 125301010, null,             "Casinos & Gaming",                               "Owners and operators of casinos and gaming facilities. Includes companies providing lottery and betting services." ),
( 125301020, null,             "Hotels, Resorts & Cruise Lines",                 "Owners and operators of hotels, resorts and cruise-ships. Includes travel agencies, tour operators and related services not classified elsewhere . Excludes casino-hotels classified in the Casinos & Gaming Sub-Industry." ),
( 125301030, null,             "Leisure Facilities",                             "Owners and operators of leisure facilities, including sport and fitness centers, stadiums, golf courses and amusement parks not classified in the Movies & Entertainment Sub-Industry." ),
( 125301040, null,             "Restaurants",                                    "Owners and operators of restaurants, bars, pubs, fast-food or take-out facilities. Includes companies that provide food catering services." ),
( 125302010, null,             "Education Services",                             "Companies providing educational services, either on-line or through conventional teaching methods. Includes, private universities, correspondence teaching, providers of educational seminars, educational materials and technical education. Excludes companies providing employee education programs classified in the Human Resources & Employment Services Sub-Industry" ),
( 125302020, null,             "Specialized Consumer Services",                  "Companies providing consumer services not classified elsewhere.  Includes residential services, home security, legal services, personal services, renovation & interior design services, consumer auctions and wedding & funeral services." ),
( 125401010, null,             "Advertising",                                    "Companies providing advertising, marketing or public relations services." ),
( 125401020, null,             "Broadcasting",                                   "Owners and operators of television or radio broadcasting systems, including programming. Includes, radio and television broadcasting, radio networks, and radio stations." ),
( 125401025, null,             "Cable & Satellite",                              "Providers of cable or satellite television services. Includes cable networks and program distribution." ),
( 125401030, null,             "Movies & Entertainment",                         "Companies that engage in producing and selling entertainment products and services, including companies engaged in the production, distribution and screening of movies and television shows, producers and distributors of music, entertainment theaters and sports teams." ),
( 125401040, null,             "Publishing",                                     "Publishers of newspapers, magazines and books, and providers of information in print or electronic formats." ),
( 125501010, null,             "Distributors",                                   "Distributors and wholesalers of general merchandise not classified elsewhere. Includes vehicle distributors." ),
( 125502010, null,             "Catalog Retail",                                 "Mail order and TV home shopping retailers. Includes companies that provide door-to-door retail." ),
( 125502020, null,             "Internet Retail",                                "Companies providing retail services primarily on the internet, not classified elsewhere." ),
( 125503010, null,             "Department Stores",                              "Owners and operators of department stores." ),
( 125503020, null,             "General Merchandise Stores",                     "Owners and operators of stores offering diversified general merchandise. Excludes hypermarkets and large-scale super centers classified in the Hypermarkets & Super Centers Sub-Industry." ),
( 125504010, null,             "Apparel Retail",                                 "Retailers specialized mainly in apparel and accessories." ),
( 125504020, null,             "Computer & Electronics Retail",                  "Owners and operators of consumer electronics, computers, video and related products retail stores." ),
( 125504030, null,             "Home Improvement Retail",                        "Owners and operators of home and garden improvement retail stores. Includes stores offering building materials and supplies." ),
( 125504040, null,             "Specialty Stores",                               "Owners and operators of specialty retail stores not classified elsewhere. Includes jewelry stores, toy stores, office supply stores, health & vision care stores, and book & entertainment stores." ),
( 125504050, null,             "Automotive Retail",                              "Owners and operators of stores specializing in automotive retail.  Includes auto dealers, gas stations, and retailers of auto accessories, motorcycles & parts, automotive glass, and automotive equipment & parts." ),
( 125504060, null,             "Homefurnishing Retail",                          "Owners and operators of furniture and home furnishings retail stores.  Includes residential furniture, homefurnishings, housewares, and interior design.  Excludes home and garden improvement stores, classified in the Home Improvement Retail Sub-Industry." ),
( 130101010, null,             "Drug Retail",                                    "Owners and operators of primarily drug retail stores and pharmacies." ),
( 130101020, null,             "Food Distributors",                              "Distributors of food products to other companies and not directly to the consumer." ),
( 130101030, null,             "Food Retail",                                    "Owners and operators of primarily food retail stores." ),
( 130101040, null,             "Hypermarkets & Super Centers",                   "Owners and operators of hypermarkets and super centers selling food and a wide-range of consumer staple products.  Excludes Food and Drug Retailers classified in the Food Retail and Drug Retail Sub-Industries, respectively." ),
( 130201010, null,             "Brewers",                                        "Producers of beer and malt liquors. Includes breweries not classified in the Restaurants Sub-Industry." ),
( 130201020, null,             "Distillers & Vintners",                          "Distillers, vintners and producers of alcoholic beverages not classified in the Brewers Sub-Industry." ),
( 130201030, null,             "Soft Drinks",                                    "Producers of non-alcoholic beverages including mineral waters. Excludes producers of milk classified in the Packaged Foods Sub-Industry." ),
( 130202010, null,             "Agricultural Products",                          "Producers of agricultural products. Includes crop growers, owners of plantations and companies that produce and process foods but do not package and market them. Excludes companies classified in the Forest Products Sub-Industry and those that package and market the food products classified in the Packaged Foods Sub-Industry." ),
( 130202020, null,             "Meat, Poultry & Fish",                           "EXP March 28 2002 -- Companies that raise livestock or poultry, fishing companies and other producers of meat, poultry or fish products." ),
( 130202030, null,             "Packaged Foods & Meats",                         "Producers of packaged foods including dairy products, fruit juices, meats, poultry, fish and pet foods." ),
( 130203010, null,             "Tobacco",                                        "Manufacturers of cigarettes and other tobacco products." ),
( 130301010, null,             "Household Products",                             "Producers of non-durable household products, including detergents, soaps, diapers and other tissue and household paper products not classified in the Paper Products Sub-Industry." ),
( 130302010, null,             "Personal Products",                              "Manufacturers of personal and beauty care products, including cosmetics and perfumes." ),
( 135101010, null,             "Health Care Equipment",                          "Manufacturers of health care equipment and devices. Includes medical instruments, drug delivery systems, cardiovascular & orthopedic devices, and diagnostic equipment." ),
( 135101020, null,             "Health Care Supplies",                           "Manufacturers of health care supplies and medical products not classified elsewhere. Includes eye care products, hospital supplies, and safety needle & syringe devices." ),
( 135102010, null,             "Health Care Distributors",                       "Distributors and wholesalers of health care products not classified elsewhere." ),
( 135102015, null,             "Health Care Services",                           "Providers of patient health care services not classified elsewhere. Includes dialysis centers, lab testing services, and pharmacy management services. Also includes companies providing business support services to health care providers, such as clerical support services, collection agency services, staffing services and outsourced sales & marketing services" ),
( 135102020, null,             "Health Care Facilities",                         "Owners and operators of health care facilities, including hospitals, nursing homes, rehabilitation centers and animal hospitals." ),
( 135102030, null,             "Managed Health Care",                            "Owners and operators of Health Maintenance Organizations (HMOs) and other managed plans." ),
( 135103010, null,             "Health Care Technology",                         "Companies providing information technology services primarily to health care providers.  Includes companies providing application, systems and/or data processing software, internet-based tools, and IT consulting services to doctors, hospitals or businesses operating primarily in the Health Care Sector" ),
( 135201010, null,             "Biotechnology",                                  "Companies primarily engaged in the research, development, manufacturing and/or marketing of products based on genetic analysis and genetic engineering.  Includes companies specializing in protein-based therapeutics to treat human diseases" ),
( 135202010, null,             "Pharmaceuticals",                                "Companies engaged in the research, development or production of pharmaceuticals. Includes veterinary drugs." ),
( 135203010, null,             "Life Sciences Tools & Services",                 "Companies enabling the drug discovery, development and production continuum by providing analytical tools, instruments, consumables & supplies, clinical trial services and contract research services.  Includes firms primarily servicing the pharmaceutical and biotechnology industries." ),
( 140101010, null,             "Diversified Banks",                              "Commercial banks whose businesses are derived primarily from commercial lending operations and have significant business activity in retail banking and small and medium corporate lending.  Excludes banks classified in the Regional Banks and Thrifts & Mortgage Finance sub-industries. Also excludes investment banks classified in the Investment Banking & Brokerage Sub-Industry." ),
( 140101015, null,             "Regional Banks",                                 "Commercial banks whose businesses are derived primarily from commercial lending operations and have significant business activity in retail banking and small and medium corporate lending. Regional banks tend to operate in limited geographic regions. Excludes companies classified in the Diversified Banks and Thrifts & Mortgage Banks sub-industries. Also excludes investment banks classified in the Investment Banking & Brokerage Sub-Industry." ),
( 140102010, null,             "Thrifts & Mortgage Finance",                     "Financial institutions providing mortgage and mortgage related services.  These include financial institutions whose assets are primarily mortgage related, savings & loans, mortgage GSE's (government sponsored enterprises), mortgage lending institutions, building societies and companies providing insurance to mortgage banks." ),
( 140201010, null,             "Consumer Finance",                               "EXP 04/30/2003 -- Providers of consumer finance services, including personal credit, credit cards, lease financing, mortgage lenders, travel-related money services and pawn shops." ),
( 140201020, null,             "Other Diversified Financial Services",           "Providers of a diverse range of financial services and/or with some interest in a wide range of financial services including banking, insurance and capital markets, but with no dominant business line." ),
( 140201030, null,             "Multi-Sector Holdings",                          "A company with significantly diversified holdings across three or more sectors, none of which contributes a majority of profit and/or sales. Stakes held are predominantly of a non-controlling nature.  Includes diversified financial companies where stakes held are of a controlling nature. Excludes other diversified companies classified in the Industrials Conglomerates Sub-Industry." ),
( 140201040, null,             "Specialized Finance",                            "Providers of specialized financial services. Includes credit agencies, stock exchanges and specialty boutiques. Companies in this Sub-Industry derive a majority of revenue from one, specialized line of business." ),
( 140202010, null,             "Consumer Finance",                               "Providers of consumer finance services, including personal credit, credit cards, lease financing, travel-related money services and pawn shops.  Excludes mortgage lenders classified in the Thrifts & Mortgage Banks Sub-Industry." ),
( 140203010, null,             "Asset Management & Custody Banks",               "Financial institutions primarily engaged in investment management and/or related custody and securities fee-based services. Includes companies operating mutual funds, closed-end funds and unit investment trusts.  Excludes banks and other financial institutions primarily involved in commercial lending, investment banking, brokerage and other specialized financial activities." ),
( 140203020, null,             "Investment Banking & Brokerage",                 "Financial institutions primarily engaged in investment banking & brokerage services, including equity and debt underwriting, mergers and acquisitions, securities lending and advisory services. Excludes banks and other financial institutions primarily involved in commercial lending, asset management and specialized financial activities." ),
( 140203030, null,             "Diversified Capital Markets",                    "Financial institutions primarily engaged in diversified capital markets activities, including a significant presence in at least two of the following area: large/major corporate lending, investment banking, brokerage and asset management. Excludes less diversified companies classified in the Asset Management & Custody Banks or Investment Banking & Brokerage sub-industries.  Also excludes companies classified in the Banks or Insurance industry groups or the Consumer Finance Sub-Industry." ),
( 140301010, null,             "Insurance Brokers",                              "Insurance and reinsurance brokerage firms." ),
( 140301020, null,             "Life & Health Insurance",                        "Companies providing primarily life, disability, indemnity or supplemental health insurance. Excludes managed care companies classified in the Managed Health Care Sub-Industry." ),
( 140301030, null,             "Multi-line Insurance",                           "Insurance companies with diversified interests in life, health and property and casualty insurance." ),
( 140301040, null,             "Property & Casualty Insurance",                  "Companies providing primarily property and casualty insurance." ),
( 140301050, null,             "Reinsurance",                                    "Companies providing primarily reinsurance." ),
( 140401010, null,             "Real Estate Investment Trusts",                  "EXP 04/28/2006 -- Real estate investment trusts (REITs).  Includes Property Trusts." ),
( 140401020, null,             "Real Estate Management & Development",           "EXP 04/28/2006 -- Companies engaged in real estate ownership, development or  management." ),
( 140402010, null,             "Diversified REIT's",                             "A company or Trust with significantly diversified operations across two or more property types." ),
( 140402020, null,             "Industrial REIT's",                              "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of industrial properties. Includes companies operating industrial warehouses and distribution properties." ),
( 140402030, null,             "Mortgage REIT's",                                "Companies or Trusts that service, originate, purchase and/or securitize residential and/or commercial mortgage loans.  Includes trusts that invest in mortgage-backed securities and other mortgage related assets." ),
( 140402040, null,             "Office REIT's",                                  "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of office properties." ),
( 140402050, null,             "Residential REIT's",                             "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of residential properties including multifamily homes, apartments, manufactured homes and student housing properties" ),
( 140402060, null,             "Retail REIT's",                                  "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of shopping malls, outlet malls, neighborhood and community shopping centers." ),
( 140402070, null,             "Specialized REIT's",                             "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of properties not classified elsewhere. Includes trusts that operate and invest in health care, leisure, hotel/resort and storage properties. It also includes REITs that do not generate a majority of their revenues and income from real estate rental and leasing operations." ),
( 140403010, null,             "Diversified Real Estate Activities",             "Companies engaged in a diverse spectrum of real estate activities including real estate development & sales, real estate management, or real estate services, but with no dominant business line." ),
( 140403020, null,             "Real Estate Operating Companies",                "Companies engaged in operating real estate properties for the purpose of leasing & management." ),
( 140403030, null,             "Real Estate Development",                        "Companies that develop real estate and sell the properties after development. Excludes companies classified in the Homebuilding Sub-Industry." ),
( 140403040, null,             "Real Estate Services",                           "Real estate service providers such as real estate agents, brokers & real estate appraisers." ),
( 145101010, null,             "Internet Software & Services",                   "Companies developing and marketing internet software and/or providing internet services including online databases and interactive services, web address registration services, database construction and internet design services. Excludes companies classified in the Internet Retail Sub-Industry." ),
( 145102010, null,             "IT Consulting & Other Services",                 "Providers of information technology and systems integration services not classified in the Data Processing & Outsourced Services or Internet Software & Services Sub-Industries.  Includes information technology consulting and information management services." ),
( 145102020, null,             "Data Processing & Outsourced Services",          "Providers of commercial electronic data processing and/or business process outsourcing services.  Includes companies that provide services for back-office automation." ),
( 145103010, null,             "Application Software",                           "Companies engaged in developing and producing software designed for specialized applications for the business or consumer market. Includes enterprise and technical software. Excludes companies classified in the Home Entertainment Software Sub-Industry. Also excludes companies producing systems or database management software classified in the Systems Software Sub-Industry." ),
( 145103020, null,             "Systems Software",                               "Companies engaged in developing and producing systems and database management software." ),
( 145103030, null,             "Home Entertainment Software",                    "Manufacturers of home entertainment software and educational software used primarily in the home." ),
( 145201020, null,             "Communications Equipment",                       "Manufacturers of communication equipment and products, including LANs, WANs, routers, telephones, switchboards and exchanges." ),
( 145201010, null,             "Networking Equipment",                           "EXP 04/30/2003. -- Manufacturers of computer networking equipment and products, including LANs, WANs and routers." ),
( 145201020, null,             "Telecommunications Equipment",                   "EXP 04/30/2003. -- Manufacturers of telecommunication equipment, including telephones, switchboards and exchanges. Excludes companies classified in the Networking Equipment Sub-Industry." ),
( 145202010, null,             "Computer Hardware",                              "Manufacturers of personal computers, servers, mainframes and workstations. Includes manufacturers of Automatic Teller Machines (ATMs). Excludes manufacturers of copiers, faxes and related products classified in the Office Electronics Sub-Industry." ),
( 145202020, null,             "Computer Storage & Peripherals",                 "Manufacturers of electronic computer components and peripherals. Includes data storage components, motherboards, audio and video cards, monitors, keyboards, printers and other peripherals. Excludes semiconductors classified in the Semiconductors Sub-Industry." ),
( 145203010, null,             "Electronic Equipment & Instruments ",            "Manufacturers of electronic equipment and instruments including analytical, electronic test and measurement instruments, scanner/barcode products, lasers, display screens, point-of-sales machines, and security system equipment." ),
( 145203015, null,             "Electronic Components",                          "Manufacturers of electronic components. Includes electronic components, connection devices, electron tubes, electronic capacitors and resistors, electronic coil, printed circuit board, transformer and other inductors, signal processing technology/components." ),
( 145203020, null,             "Electronic Manufacturing Services",              "Producers of electronic equipment mainly for the OEM (Original Equipment Manufacturers) markets." ),
( 145203030, null,             "Technology Distributors",                        "Distributors of technology hardware and equipment. Includes distributors of communications equipment, computers & peripherals, semiconductors, and electronic equipment and components." ),
( 145204010, null,             "Office Electronics",                             "Manufacturers of office electronic equipment including copiers and faxes." ),
( 145205010, null,             "Semiconductor Equipment",                        "EXP 04/30/2003. -- Manufacturers of semiconductor equipment." ),
( 145205020, null,             "Semiconductors",                                 "EXP 04/30/2003. -- Manufacturers of semiconductors and related products." ),
( 145301010, null,             "Semiconductor Equipment",                        "Manufacturers of semiconductor equipment, including manufacturers of the raw material and equipment used in the solar power industry." ),
( 145301020, null,             "Semiconductors",                                 "Manufacturers of semiconductors and related products, including manufacturers of solar modules and cells." ),
( 150101010, null,             "Alternative Carriers",                           "Providers of communications and high-density data transmission services primarily through a high bandwidth/fiber-optic cable network." ),
( 150101020, null,             "Integrated Telecommunication Services",          "Operators of primarily fixed-line telecommunications networks and companies providing both wireless and fixed-line telecommunications services not classified elsewhere." ),
( 150102010, null,             "Wireless Telecommunication Services",            "Providers of primarily cellular or wireless telecommunication services, including paging services." ),
( 155101010, null,             "Electric Utilities",                             "Companies that produce or distribute electricity. Includes both nuclear and non-nuclear facilities." ),
( 155102010, null,             "Gas Utilities",                                  "Companies whose main charter is to distribute and transmit natural and manufactured gas. Excludes companies primarily involved in gas exploration or production classified in the Oil & Gas Exploration & Production Sub-Industry.   Also excludes diversified midstream natural gas companies classified in the Oil & Gas Storage & Transportation Sub-Industry." ),
( 155103010, null,             "Multi-Utilities",                                "Utility companies with significantly diversified activities in addition to core Electric Utility, Gas Utility and/or Water Utility operations." ),
( 155104010, null,             "Water Utilities",                                "Companies that purchase and redistribute water to the end-consumer. Includes large-scale water treatment systems." ),
( 155105010, null,             "Independent Power Producers & Energy Traders",   "Companies that operate as Independent Power Producers (IPPs), Gas & Power Marketing & Trading Specialists and/or Integrated Energy Merchants. Includes producers of solar power and wind power, used to generate electricity. Also includes companies that generate electricity and/or power through alternative energy sources such as biogas, biomass, clean energy, geothermal, waste, water and waves.  Excludes electric transmission companies and utility distribution companies classified in the Electric Utilities Sub-Industry." ),
( 200000001, "gov tech",       "Defense & Space",                                null ),
( 200000003, "tech",           "Computer Hardware",                              null ),
( 200000004, "tech",           "Computer Software",                              null ),
( 200000005, "tech",           "Computer Networking",                            null ),
( 200000006, "tech",           "Internet",                                       null ),
( 200000007, "tech",           "Semiconductors",                                 null ),
( 200000008, "gov tech",       "Telecommunications",                             null ),
( 200000009, "leg",            "Law Practice",                                   null ),
( 200000010, "leg",            "Legal Services",                                 null ),
( 200000011, "corp",           "Management Consulting",                          null ),
( 200000012, "gov hlth tech",  "Biotechnology",                                  null ),
( 200000013, "hlth",           "Medical Practice",                               null ),
( 200000014, "hlth",           "Hospital & Health Care",                         null ),
( 200000015, "hlth tech",      "Pharmaceuticals",                                null ),
( 200000016, "hlth",           "Veterinary",                                     null ),
( 200000017, "hlth",           "Medical Devices",                                null ),
( 200000018, "good",           "Cosmetics",                                      null ),
( 200000019, "good",           "Apparel & Fashion",                              null ),
( 200000020, "good rec",       "Sporting Goods",                                 null ),
( 200000021, "good",           "Tobacco",                                        null ),
( 200000022, "good",           "Supermarkets",                                   null ),
( 200000023, "good man serv",  "Food Production",                                null ),
( 200000024, "good man",       "Consumer Electronics",                           null ),
( 200000025, "good man",       "Consumer Goods",                                 null ),
( 200000026, "good man",       "Furniture",                                      null ),
( 200000027, "good man",       "Retail",                                         null ),
( 200000028, "med rec",        "Entertainment",                                  null ),
( 200000029, "rec",            "Gambling & Casinos",                             null ),
( 200000030, "rec serv tran",  "Leisure, Travel & Tourism",                      null ),
( 200000031, "rec serv tran",  "Hospitality",                                    null ),
( 200000032, "rec serv",       "Restaurants",                                    null ),
( 200000033, "rec",            "Sports",                                         null ),
( 200000034, "rec serv",       "Food & Beverages",                               null ),
( 200000035, "art med rec",    "Motion Pictures and Film",                       null ),
( 200000036, "med rec",        "Broadcast Media",                                null ),
( 200000037, "art med rec",    "Museums and Institutions",                       null ),
( 200000038, "art med rec",    "Fine Art",                                       null ),
( 200000039, "art med rec",    "Performing Arts",                                null ),
( 200000040, "rec serv",       "Recreational Facilities and Services",           null ),
( 200000041, "fin",            "Banking",                                        null ),
( 200000042, "fin",            "Insurance",                                      null ),
( 200000043, "fin",            "Financial Services",                             null ),
( 200000044, "cons fin good",  "Real Estate",                                    null ),
( 200000045, "fin",            "Investment Banking",                             null ),
( 200000046, "fin",            "Investment Management",                          null ),
( 200000047, "corp fin",       "Accounting",                                     null ),
( 200000048, "cons",           "Construction",                                   null ),
( 200000049, "cons",           "Building Materials",                             null ),
( 200000050, "cons",           "Architecture & Planning",                        null ),
( 200000051, "cons gov",       "Civil Engineering",                              null ),
( 200000052, "gov man",        "Aviation & Aerospace",                           null ),
( 200000053, "man",            "Automotive",                                     null ),
( 200000054, "man",            "Chemicals",                                      null ),
( 200000055, "man",            "Machinery",                                      null ),
( 200000056, "man",            "Mining & Metals",                                null ),
( 200000057, "man",            "Oil & Energy",                                   null ),
( 200000058, "man",            "Shipbuilding",                                   null ),
( 200000059, "man",            "Utilities",                                      null ),
( 200000060, "man",            "Textiles",                                       null ),
( 200000061, "man",            "Paper & Forest Products",                        null ),
( 200000062, "man",            "Railroad Manufacture",                           null ),
( 200000063, "agr",            "Farming",                                        null ),
( 200000064, "agr",            "Ranching",                                       null ),
 ( 200000065, "agr",            "Dairy",                                         null ),
( 200000066, "agr",            "Fishery",                                        null ),
( 200000068, "edu",            "Higher Education",                               null ),
( 200000067, "edu",            "Primary/Secondary Education",                    null ),
( 200000069, "edu",            "Education Management",                           null ),
( 200000070, "edu gov",        "Research",                                       null ),
( 200000071, "gov",            "Military",                                       null ),
( 200000072, "gov leg",        "Legislative Office",                             null ),
( 200000073, "gov leg",        "Judiciary",                                      null ),
( 200000074, "gov",            "International Affairs",                          null ),
( 200000075, "gov",            "Government Administration",                      null ),
( 200000076, "gov",            "Executive Office",                               null ),
( 200000077, "gov leg",        "Law Enforcement",                                null ),
( 200000078, "gov",            "Public Safety",                                  null ),
( 200000079, "gov",            "Public Policy",                                  null ),
( 200000080, "corp med",       "Marketing and Advertising",                      null ),
( 200000081, "med rec",        "Newspapers",                                     null ),
( 200000082, "med rec",        "Publishing",                                     null ),
( 200000083, "med rec",        "Printing",                                       null ),
( 200000084, "med serv",       "Information Services",                           null ),
( 200000085, "med rec serv",   "Libraries",                                      null ),
( 200000086, "org serv",       "Environmental Services",                         null ),
( 200000087, "serv tran",      "Package/Freight Delivery",                       null ),
( 200000088, "org serv",       "Individual & Family Services",                   null ),
( 200000089, "org serv",       "Religious Institutions",                         null ),
( 200000090, "org serv",       "Civic & Social Organization",                    null ),
( 200000091, "org serv",       "Consumer Services",                              null ),
( 200000092, "tran",           "Transportation/Trucking/Railroad",               null ),
( 200000093, "tran",           "Warehousing",                                    null ),
( 200000094, "man tech tran",  "Airlines/Aviation",                              null ),
( 200000095, "tran",           "Maritime",                                       null ),
( 200000096, "tech",           "Information Technology and Services",            null ),
( 200000097, "corp",           "Market Research",                                null ),
( 200000098, "corp",           "Public Relations and Communications",            null ),
( 200000099, "art med",        "Design",                                         null ),
( 200000100, "org",            "Non-Profit Organization Management",             null ),
( 200000101, "org",            "Fund-Raising",                                   null ),
( 200000102, "corp org",       "Program Development",                            null ),
( 200000103, "art med rec",    "Writing and Editing",                            null ),
( 200000104, "corp",           "Staffing and Recruiting",                        null ),
( 200000105, "corp",           "Professional Training & Coaching",               null ),
( 200000106, "fin tech",       "Venture Capital & Private Equity",               null ),
( 200000107, "gov org",        "Political Organization",                         null ),
( 200000108, "corp gov serv",  "Translation and Localization",                   null ),
( 200000109, "med rec",        "Computer Games",                                 null ),
( 200000110, "corp rec serv",  "Events Services",                                null ),
( 200000111, "art med rec",    "Arts and Crafts",                                null ),
( 200000112, "good man",       "Electrical/Electronic Manufacturing",            null ),
( 200000113, "med",            "Online Media",                                   null ),
( 200000114, "gov man tech",   "Nanotechnology",                                 null ),
( 200000115, "art rec",        "Music",                                          null ),
( 200000116, "corp tran",      "Logistics and Supply Chain",                     null ),
( 200000117, "man",            "Plastics",                                       null ),
( 200000118, "tech",           "Computer & Network Security",                    null ),
( 200000119, "tech",           "Wireless",                                       null ),
( 200000120, "leg org",        "Alternative Dispute Resolution",                 null ),
( 200000121, "corp org serv",  "Security and Investigations",                    null ),
( 200000122, "corp serv",      "Facilities Services",                            null ),
( 200000123, "corp",           "Outsourcing/Offshoring",                         null ),
( 200000124, "hlth rec",       "Health, Wellness and Fitness",                   null ),
( 200000125, "hlth",           "Alternative Medicine",                           null ),
( 200000126, "med rec",        "Media Production",                               null ),
( 200000127, "art med",        "Animation",                                      null ),
( 200000128, "cons corp fin",  "Commercial Real Estate",                         null ),
( 200000129, "fin",            "Capital Markets",                                null ),
( 200000130, "gov org",        "Think Tanks",                                    null ),
( 200000131, "org",            "Philanthropy",                                   null ),
( 200000132, "edu org",        "E-Learning",                                     null ),
( 200000133, "good",           "Wholesale",                                      null ),
( 200000134, "corp good tran", "Import and Export",                              null ),
( 200000135, "cons gov man",   "Mechanical or Industrial Engineering",           null ),
( 200000136, "art med rec",    "Photography",                                    null ),
( 200000137, "corp",           "Human Resources",                                null ),
( 200000138, "corp man",       "Business Supplies and Equipment",                null ),
( 200000139, "hlth",           "Mental Health Care",                             null ),
( 200000140, "art med",        "Graphic Design",                                 null ),
( 200000141, "gov org tran",   "International Trade and Development",            null ),
( 200000142, "good man rec",   "Wine and Spirits",                               null ),
( 200000143, "good",           "Luxury Goods & Jewelry",                         null ),
( 200000144, "gov man org",    "Renewables & Environment",                       null ),
( 200000145, "cons man",       "Glass, Ceramics & Concrete",                     null ),
( 200000146, "good man",       "Packaging and Containers",                       null ),
( 200000147, "cons man",       "Industrial Automation",                          null ),
( 200000148, "gov",            "Government Relations",                           null ) )
}
