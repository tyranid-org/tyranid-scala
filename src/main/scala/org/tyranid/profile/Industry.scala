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
import org.tyranid.db.{ DbInt, DbLong, DbChar }
import org.tyranid.db.mongo.Imp._
import org.tyranid.db.mongo.{ MongoEntity }
import org.tyranid.db.meta.AutoIncrement
import org.tyranid.db.ram.RamEntity


object Industry extends MongoEntity( tid = "a0Ot" ) {
	override lazy val dbName = "businessCategories"

  "sectorId"        is DbInt               ;
  "sectorName"      is DbChar(30)          ;
  "industryGroupId" is DbInt               ;
  "industryGroup"   is DbChar(30)          ;
  "industryId"      is DbInt               ;
  "industry"        is DbChar(40)          ;
  "subIndustryId"   is DbLong              ;//is Req( UserRole.Admin ); // subIndustryId
//  "id"              is DbLong is 'key      is Req( UserRole.Admin ); // subIndustryId
  "subIndustry"     is DbChar(50) is 'label;
  "description"     is DbChar(400)         ;

  def cache = mutable.HashMap[ObjectId,DBObject]()

  def get( id:ObjectId ) = cache.getOrElseUpdate( id, Industry.db.findOne( id ) )
}

object GICS extends RamEntity( tid = "a0Ov" ) {
	override lazy val dbName = "businessCategories"

  "id"              is DbLong              ;

  "sectorId"        is DbInt               ;
  "sectorName"      is DbChar(30)          ;
  "industryGroupId" is DbInt               ;
  "industryGroup"   is DbChar(30)          ;
  "industryId"      is DbInt               ;
  "industry"        is DbChar(40)          ;
  "subIndustryId"   is DbLong              ;//is Req( UserRole.Admin ); // subIndustryId
  "subIndustry"     is DbChar(50)          is 'label;
  "description"     is DbChar(400)         ;

  static(
( "id",     "sectorId","sectorName",        "industryGroupId","industryGroup",                    "industryId","industry",                                "subIndustryId","subIndustry",                            "description" ),
( 10101010, 10,"Energy",                    1010,"Energy",                                        101010,"Energy Equipment & Services",                   10101010,"Oil & Gas Drilling",                            "Drilling contractors or owners of drilling rigs that contract their services for drilling wells" ),
( 10101020, 10,"Energy",                    1010,"Energy",                                        101010,"Energy Equipment & Services",                   10101020,"Oil & Gas Equipment & Services",                "Manufacturers of equipment, including drilling rigs and equipment, and providers of supplies and services to companies involved in the drilling, evaluation and completion of oil and gas wells." ),
( 10102010, 10,"Energy",                    1010,"Energy",                                        101020,"Oil, Gas & Consumable Fuels",                   10102010,"Integrated Oil & Gas",                          "Integrated oil companies engaged in the exploration & production of oil and gas, as well as at least one other significant activity in either refining, marketing and transportation, or chemicals." ),
( 10102020, 10,"Energy",                    1010,"Energy",                                        101020,"Oil, Gas & Consumable Fuels",                   10102020,"Oil & Gas Exploration & Production",            "Companies engaged in the exploration and production of oil and gas not classified elsewhere." ),
( 10102030, 10,"Energy",                    1010,"Energy",                                        101020,"Oil, Gas & Consumable Fuels",                   10102030,"Oil & Gas Refining & Marketing",                "Companies engaged in the refining and marketing of oil, gas and/or refined products not classified in the Integrated Oil & Gas or Independent Power Producers & Energy Traders Sub-Industries." ),
( 10102040, 10,"Energy",                    1010,"Energy",                                        101020,"Oil, Gas & Consumable Fuels",                   10102040,"Oil & Gas Storage & Transportation",            "Companies engaged in the storage and/or transportation of oil, gas and/or refined products. Includes diversified midstream natural gas companies facing competitive markets, oil and refined product pipelines, coal slurry pipelines and oil & gas shipping companies." ),
( 10102050, 10,"Energy",                    1010,"Energy",                                        101020,"Oil, Gas & Consumable Fuels",                   10102050,"Coal & Consumable Fuels",                       "Companies primarily involved in the production and mining of coal, related products and other consumable fuels related to the generation of energy.  Excludes companies primarily producing gases classified in the Industrial Gases sub-industry and companies primarily mining for metallurgical (coking) coal used for steel production." ),
( 151010,   15,"Materials",                 1510,"Materials",                                     151010,"Chemicals",                                     null,    null,                                            "Manufacturers of chemicals." ),
( 15101010, 15,"Materials",                 1510,"Materials",                                     151010,"Chemicals",                                     15101010,"Commodity Chemicals",                           "Companies that primarily produce industrial chemicals and basic chemicals. Including but not limited to plastics, synthetic fibers, films, commodity-based paints & pigments, explosives and petrochemicals. Excludes chemical companies classified in the Diversified Chemicals, Fertilizers & Agricultural Chemicals, Industrial Gases, or Specialty Chemicals Sub-Industries." ),
( 15101020, 15,"Materials",                 1510,"Materials",                                     151010,"Chemicals",                                     15101020,"Diversified Chemicals",                         "Manufacturers of a diversified range of chemical products not classified in the Industrial Gases, Commodity Chemicals, Specialty Chemicals or Fertilizers & Agricultural Chemicals Sub-Industries." ),
( 15101030, 15,"Materials",                 1510,"Materials",                                     151010,"Chemicals",                                     15101030,"Fertilizers & Agricultural Chemicals",          "Producers of fertilizers, pesticides, potash or other agriculture-related chemicals not classified elsewhere." ),
( 15101040, 15,"Materials",                 1510,"Materials",                                     151010,"Chemicals",                                     15101040,"Industrial Gases",                              "Manufacturers of industrial gases." ),
( 15101050, 15,"Materials",                 1510,"Materials",                                     151010,"Chemicals",                                     15101050,"Specialty Chemicals",                           "Companies that primarily produce high value-added chemicals used in the manufacture of a wide variety of products, including but not limited to fine chemicals, additives, advanced polymers, adhesives, sealants and specialty paints, pigments and coatings." ),
( 15102010, 15,"Materials",                 1510,"Materials",                                     151020,"Construction Materials",                        15102010,"Construction Materials",                        "Manufacturers of construction materials including sand, clay, gypsum, lime, aggregates, cement, concrete and bricks. Other finished or semi-finished building materials are classified  in the Building Products Sub-Industry." ),
( 15103010, 15,"Materials",                 1510,"Materials",                                     151030,"Containers & Packaging",                        15103010,"Metal & Glass Containers",                      "Manufacturers of metal, glass or plastic containers. Includes corks and caps." ),
( 15103020, 15,"Materials",                 1510,"Materials",                                     151030,"Containers & Packaging",                        15103020,"Paper Packaging",                               "Manufacturers of paper and cardboard containers and packaging." ),
( 15104010, 15,"Materials",                 1510,"Materials",                                     151040,"Metals & Mining",                               15104010,"Aluminum",                                      "Producers of aluminum and related products, including companies that mine or process bauxite and companies that recycle aluminum to produce finished or semi-finished products. Excludes companies that primarily produce aluminum building materials classified in the Building Products Sub-Industry." ),
( 15104020, 15,"Materials",                 1510,"Materials",                                     151040,"Metals & Mining",                               15104020,"Diversified Metals & Mining",                   "Companies engaged in the diversified production or extraction of metals and minerals not classified elsewhere. Including, but not limited to, nonferrous metal mining (except aluminum), salt and borate mining, phosphate rock mining, metallurgical (coking) coal mining used for steel production and diversified mining operations. Excludes iron ore mining, classified in the Steel Sub-Industry and aluminum mining classified in the Aluminum Sub-Industry.  Excludes bituminous (thermal) coal-mining companies classified in the Coal & Consumable Fuels Sub-Industry." ),
( 15104030, 15,"Materials",                 1510,"Materials",                                     151040,"Metals & Mining",                               15104030,"Gold",                                          "Producers of gold and related products, including companies that mine or process gold and the South African finance houses which primarily invest in, but do not operate, gold mines." ),
( 15104040, 15,"Materials",                 1510,"Materials",                                     151040,"Metals & Mining",                               15104040,"Precious Metals & Minerals",                    "Companies mining precious metals and minerals not classified in the Gold Sub-Industry. Includes companies primarily mining platinum." ),
( 15104050, 15,"Materials",                 1510,"Materials",                                     151040,"Metals & Mining",                               15104050,"Steel",                                         "Producers of iron and steel and related products" ),
( 15105010, 15,"Materials",                 1510,"Materials",                                     151050,"Paper & Forest Products",                       15105010,"Forest Products",                               "Manufacturers of timber and related wood products. Includes lumber for the building industry." ),
( 15105020, 15,"Materials",                 1510,"Materials",                                     151050,"Paper & Forest Products",                       15105020,"Paper Products",                                "Manufacturers of all grades of paper. Excludes companies specializing in paper packaging classified in the Paper Packaging Sub-Industry." ),
( 20101010, 20,"Industrials",               2010,"Capital Goods",                                 201010,"Aerospace & Defense",                           20101010,"Aerospace & Defense",                           "Manufacturers of civil or military aerospace and defense equipment, parts or products. Includes defense electronics and space equipment." ),
( 20102010, 20,"Industrials",               2010,"Capital Goods",                                 201020,"Building Products",                             20102010,"Building Products",                             "Manufacturers of building components and home improvement products and equipment. Excludes lumber and plywood classified under Forest Products and cement and other materials classified in the Construction Materials Sub-Industry." ),
( 20103010, 20,"Industrials",               2010,"Capital Goods",                                 201030,"Construction & Engineering",                    20103010,"Construction & Engineering",                    "Companies engaged in primarily non-residential construction. Includes civil engineering companies and large-scale contractors. Excludes companies classified in the Homebuilding Sub-Industry." ),
( 20104010, 20,"Industrials",               2010,"Capital Goods",                                 201040,"Electrical Equipment",                          20104010,"Electrical Components & Equipment",             "Companies that produce electric cables and wires, electrical components or equipment not classified in the Heavy Electrical Equipment Sub-Industry." ),
( 20104020, 20,"Industrials",               2010,"Capital Goods",                                 201040,"Electrical Equipment",                          20104020,"Heavy Electrical Equipment",                    "Manufacturers of power-generating equipment and other heavy electrical equipment, including power turbines, heavy electrical machinery intended for fixed-use and large electrical systems. Excludes cables and wires, classified in the Electrical Components & Equipment Sub-Industry." ),
( 20105010, 20,"Industrials",               2010,"Capital Goods",                                 201050,"Industrial Conglomerates",                      20105010,"Industrial Conglomerates",                      "Diversified industrial companies with business activities in three or more sectors, none of which contributes a majority of revenues. Stakes held are predominantly of a controlling nature and stake holders maintain an operational interest in the running of the subsidiaries." ),
( 20106010, 20,"Industrials",               2010,"Capital Goods",                                 201060,"Machinery",                                     20106010,"Construction & Farm Machinery & Heavy Trucks",  "Manufacturers of heavy duty trucks, rolling machinery, earth-moving and construction equipment, heavy farm machinery and manufacturers of related parts. Includes non-military shipbuilding." ),
( 20106020, 20,"Industrials",               2010,"Capital Goods",                                 201060,"Machinery",                                     20106020,"Industrial Machinery",                          "Manufacturers of industrial machinery and industrial components. Includes companies that manufacture presses, machine tools, compressors, pollution control equipment, elevators, escalators, insulators, pumps, roller bearings and other metal fabrications." ),
( 20107010, 20,"Industrials",               2010,"Capital Goods",                                 201070,"Trading Companies & Distributors",              20107010,"Trading Companies & Distributors",              "Trading companies and other distributors of industrial equipment and products." ),
( 20201010, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201010,"Commercial Printing",                           "Companies providing commercial printing services. Includes printers primarily serving the media industry." ),
( 20201020, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201020,"Data Processing Services",                      "EXP 04/30/2003 -- Providers of commercial electronic data processing services." ),
( 20201030, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201030,"Diversified Commercial & Professional Services","EXP 08/31/2008 -- Companies primarily providing commercial, industrial and professional services to businesses and governments not classified elsewhere.  Includes commercial cleaning services, consulting services, correctional facilities, dining & catering services, document & communication services, equipment repair services, security & alarm services, storage & warehousing, and uniform rental services." ),
( 20201040, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201040,"Human Resource & Employment Services",          "EXP 08/31/2008 -- Companies providing business support services relating to human capital management. Includes employment agencies, employee training, payroll & benefit support services, retirement support services and temporary agencies." ),
( 20201050, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201050,"Environmental & Facilities Services",           "Companies providing environmental and facilities maintenance services. Includes waste management, facilities management and pollution control services.  Excludes large-scale water treatment systems classified in the Water Utilities Sub-Industry." ),
( 20201060, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201060,"Office Services & Supplies",                    "Providers of office services and manufacturers of office supplies and equipment not classified elsewhere." ),
( 20201070, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201070,"Diversified Support Services",                  "Companies primarily providing labor oriented support services to businesses and governments.  Includes commercial cleaning services, dining & catering services, equipment repair services, industrial maintenance services, industrial auctioneers, storage & warehousing, transaction services, uniform rental services, and other business support services." ),
( 20201080, 20,"Industrials",               2020,"Commercial & Professional Services",            202010,"Commercial Services & Supplies",                20201080,"Security & Alarm Services",                     "Companies providing security and protection services to business and governments. Includes companies providing services such as correctional facilities, security & alarm services, armored transportation & guarding.  Excludes companies providing security software classified under the Systems Software Sub-Industry and home security services classified under the Specialized Consumer Services Sub-Industry. Also excludes companies manufacturing security system equipment classified under the Electronic Equipment & Instruments Sub-Industry. " ),
( 20202010, 20,"Industrials",               2020,"Commercial & Professional Services",            202020,"Professional Services",                         20202010,"Human Resource & Employment Services",          "Companies providing business support services relating to human capital management. Includes employment agencies, employee training, payroll & benefit support services, retirement support services and temporary agencies." ),
( 20202020, 20,"Industrials",               2020,"Commercial & Professional Services",            202020,"Professional Services",                         20202020,"Research & Consulting Services",                "Companies primarily providing research and consulting services to businesses and governments not classified elsewhere.  Includes companies involved in management consulting services, architectural design, business information or scientific research, marketing, and testing & certification services. Excludes companies providing information technology consulting services classified in the IT Consulting & Other Services Sub-Industry." ),
( 20301010, 20,"Industrials",               2030,"Transportation",                                203010,"Air Freight & Logistics",                       20301010,"Air Freight & Logistics",                       "Companies providing air freight transportation, courier and logistics services, including package and mail delivery and customs agents. Excludes those companies classified in the Airlines, Marine or Trucking Sub-Industries." ),
( 20302010, 20,"Industrials",               2030,"Transportation",                                203020,"Airlines",                                      20302010,"Airlines",                                      "Companies providing primarily passenger air transportation." ),
( 20303010, 20,"Industrials",               2030,"Transportation",                                203030,"Marine",                                        20303010,"Marine",                                        "Companies providing goods or passenger maritime transportation. Excludes cruise-ships classified in the Hotels, Resorts & Cruise Lines Sub-Industry." ),
( 20304010, 20,"Industrials",               2030,"Transportation",                                203040,"Road & Rail",                                   20304010,"Railroads",                                     "Companies providing primarily goods and passenger rail  transportation." ),
( 20304020, 20,"Industrials",               2030,"Transportation",                                203040,"Road & Rail",                                   20304020,"Trucking",                                      "Companies providing primarily goods and passenger land transportation. Includes vehicle rental and taxi companies." ),
( 20305010, 20,"Industrials",               2030,"Transportation",                                203050,"Transportation Infrastructure",                 20305010,"Airport Services",                              "Operators of airports and companies providing related services." ),
( 20305020, 20,"Industrials",               2030,"Transportation",                                203050,"Transportation Infrastructure",                 20305020,"Highways & Railtracks",                         "Owners and operators of roads, tunnels and railtracks." ),
( 20305030, 20,"Industrials",               2030,"Transportation",                                203050,"Transportation Infrastructure",                 20305030,"Marine Ports & Services",                       "Owners and operators of marine ports and related services." ),
( 25101010, 25,"Consumer Discretionary",    2510,"Automobiles & Components",                      251010,"Auto Components",                               25101010,"Auto Parts & Equipment",                        "Manufacturers of parts and accessories for  automobiles and motorcycles. Excludes companies classified in the Tires & Rubber Sub-Industry." ),
( 25101020, 25,"Consumer Discretionary",    2510,"Automobiles & Components",                      251010,"Auto Components",                               25101020,"Tires & Rubber",                                "Manufacturers of tires and rubber." ),
( 25102010, 25,"Consumer Discretionary",    2510,"Automobiles & Components",                      251020,"Automobiles",                                   25102010,"Automobile Manufacturers",                      "Companies that produce mainly passenger automobiles and light trucks. Excludes companies producing mainly motorcycles and three-wheelers classified in the Motorcycle Manufacturers Sub-Industry and heavy duty trucks classified in the Construction & Farm Machinery & Heavy Trucks Sub-Industry." ),
( 25102020, 25,"Consumer Discretionary",    2510,"Automobiles & Components",                      251020,"Automobiles",                                   25102020,"Motorcycle Manufacturers",                      "Companies that produce motorcycles, scooters or three-wheelers. Excludes bicycles classified in the Leisure Products Sub-Industry. " ),
( 25201010, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252010,"Household Durables",                            25201010,"Consumer Electronics",                          "Manufacturers of consumer electronics products including TVs, VCRs, hi-fi equipment, game consoles and related products. Excludes personal home computer manufacturers classified in the Computer Hardware Sub-Industry, and electric household appliances classified in the Household Appliances Sub-Industry." ),
( 25201020, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252010,"Household Durables",                            25201020,"Home Furnishings",                              "Manufacturers of soft home furnishings or furniture, including upholstery, carpets and wall-coverings." ),
( 25201030, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252010,"Household Durables",                            25201030,"Homebuilding",                                  "Residential construction companies. Includes manufacturers of prefabricated houses and semi-fixed manufactured homes." ),
( 25201040, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252010,"Household Durables",                            25201040,"Household Appliances",                          "Manufacturers of electric household appliances and related products.  Includes manufacturers of power and hand tools, including garden improvement tools.  Excludes TVs and other audio and video products classified in the Consumer Electronics Sub-Industry and personal computers classified in the Computer Hardware Sub-Industry." ),
( 25201050, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252010,"Household Durables",                            25201050,"Housewares & Specialties",                      "Manufacturers of durable household products, including cutlery, cookware, glassware, crystal, silverware, utensils, kitchenware and consumer specialties not classified elsewhere." ),
( 25202010, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252020,"Leisure Equipment & Products",                  25202010,"Leisure Products",                              "Manufacturers of leisure products and equipment including sports equipment, bicycles and toys." ),
( 25202020, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252020,"Leisure Equipment & Products",                  25202020,"Photographic Products",                         "Manufacturers of photographic equipment and related products." ),
( 25203010, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252030,"Textiles, Apparel & Luxury Goods",              25203010,"Apparel, Accessories & Luxury Goods",           "Manufacturers of apparel, accessories & luxury goods. Includes companies primarily producing designer handbags, wallets, luggage, jewelry and watches. Excludes shoes classified in the Footwear Sub-Industry." ),
( 25203020, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252030,"Textiles, Apparel & Luxury Goods",              25203020,"Footwear",                                      "Manufacturers of footwear. Includes sport and leather shoes." ),
( 25203030, 25,"Consumer Discretionary",    2520,"Consumer Durables & Apparel",                   252030,"Textiles, Apparel & Luxury Goods",              25203030,"Textiles",                                      "Manufacturers of textile and related products not classified in the Apparel, Accessories & Luxury Goods, Footwear or Home Furnishings Sub-Industries." ),
( 25301010, 25,"Consumer Discretionary",    2530,"Consumer Services",                             253010,"Hotels, Restaurants & Leisure",                 25301010,"Casinos & Gaming",                              "Owners and operators of casinos and gaming facilities. Includes companies providing lottery and betting services." ),
( 25301020, 25,"Consumer Discretionary",    2530,"Consumer Services",                             253010,"Hotels, Restaurants & Leisure",                 25301020,"Hotels, Resorts & Cruise Lines",                "Owners and operators of hotels, resorts and cruise-ships. Includes travel agencies, tour operators and related services not classified elsewhere . Excludes casino-hotels classified in the Casinos & Gaming Sub-Industry." ),
( 25301030, 25,"Consumer Discretionary",    2530,"Consumer Services",                             253010,"Hotels, Restaurants & Leisure",                 25301030,"Leisure Facilities",                            "Owners and operators of leisure facilities, including sport and fitness centers, stadiums, golf courses and amusement parks not classified in the Movies & Entertainment Sub-Industry." ),
( 25301040, 25,"Consumer Discretionary",    2530,"Consumer Services",                             253010,"Hotels, Restaurants & Leisure",                 25301040,"Restaurants",                                   "Owners and operators of restaurants, bars, pubs, fast-food or take-out facilities. Includes companies that provide food catering services." ),
( 25302010, 25,"Consumer Discretionary",    2530,"Consumer Services",                             253020,"Diversified Consumer Services",                 25302010,"Education Services",                            "Companies providing educational services, either on-line or through conventional teaching methods. Includes, private universities, correspondence teaching, providers of educational seminars, educational materials and technical education. Excludes companies providing employee education programs classified in the Human Resources & Employment Services Sub-Industry" ),
( 25302020, 25,"Consumer Discretionary",    2530,"Consumer Services",                             253020,"Diversified Consumer Services",                 25302020,"Specialized Consumer Services",                 "Companies providing consumer services not classified elsewhere.  Includes residential services, home security, legal services, personal services, renovation & interior design services, consumer auctions and wedding & funeral services." ),
( 25401010, 25,"Consumer Discretionary",    2540,"Media",                                         254010,"Media",                                         25401010,"Advertising",                                   "Companies providing advertising, marketing or public relations services." ),
( 25401020, 25,"Consumer Discretionary",    2540,"Media",                                         254010,"Media",                                         25401020,"Broadcasting",                                  "Owners and operators of television or radio broadcasting systems, including programming. Includes, radio and television broadcasting, radio networks, and radio stations." ),
( 25401025, 25,"Consumer Discretionary",    2540,"Media",                                         254010,"Media",                                         25401025,"Cable & Satellite",                             "Providers of cable or satellite television services. Includes cable networks and program distribution." ),
( 25401030, 25,"Consumer Discretionary",    2540,"Media",                                         254010,"Media",                                         25401030,"Movies & Entertainment",                        "Companies that engage in producing and selling entertainment products and services, including companies engaged in the production, distribution and screening of movies and television shows, producers and distributors of music, entertainment theaters and sports teams." ),
( 25401040, 25,"Consumer Discretionary",    2540,"Media",                                         254010,"Media",                                         25401040,"Publishing",                                    "Publishers of newspapers, magazines and books, and providers of information in print or electronic formats." ),
( 25501010, 25,"Consumer Discretionary",    2550,"Retailing",                                     255010,"Distributors",                                  25501010,"Distributors",                                  "Distributors and wholesalers of general merchandise not classified elsewhere. Includes vehicle distributors." ),
( 25502010, 25,"Consumer Discretionary",    2550,"Retailing",                                     255020,"Internet & Catalog Retail",                     25502010,"Catalog Retail",                                "Mail order and TV home shopping retailers. Includes companies that provide door-to-door retail." ),
( 25502020, 25,"Consumer Discretionary",    2550,"Retailing",                                     255020,"Internet & Catalog Retail",                     25502020,"Internet Retail",                               "Companies providing retail services primarily on the internet, not classified elsewhere." ),
( 25503010, 25,"Consumer Discretionary",    2550,"Retailing",                                     255030,"Multiline Retail",                              25503010,"Department Stores",                             "Owners and operators of department stores." ),
( 25503020, 25,"Consumer Discretionary",    2550,"Retailing",                                     255030,"Multiline Retail",                              25503020,"General Merchandise Stores",                    "Owners and operators of stores offering diversified general merchandise. Excludes hypermarkets and large-scale super centers classified in the Hypermarkets & Super Centers Sub-Industry." ),
( 25504010, 25,"Consumer Discretionary",    2550,"Retailing",                                     255040,"Specialty Retail",                              25504010,"Apparel Retail",                                "Retailers specialized mainly in apparel and accessories." ),
( 25504020, 25,"Consumer Discretionary",    2550,"Retailing",                                     255040,"Specialty Retail",                              25504020,"Computer & Electronics Retail",                 "Owners and operators of consumer electronics, computers, video and related products retail stores." ),
( 25504030, 25,"Consumer Discretionary",    2550,"Retailing",                                     255040,"Specialty Retail",                              25504030,"Home Improvement Retail",                       "Owners and operators of home and garden improvement retail stores. Includes stores offering building materials and supplies." ),
( 25504040, 25,"Consumer Discretionary",    2550,"Retailing",                                     255040,"Specialty Retail",                              25504040,"Specialty Stores",                              "Owners and operators of specialty retail stores not classified elsewhere. Includes jewelry stores, toy stores, office supply stores, health & vision care stores, and book & entertainment stores." ),
( 25504050, 25,"Consumer Discretionary",    2550,"Retailing",                                     255040,"Specialty Retail",                              25504050,"Automotive Retail",                             "Owners and operators of stores specializing in automotive retail.  Includes auto dealers, gas stations, and retailers of auto accessories, motorcycles & parts, automotive glass, and automotive equipment & parts." ),
( 25504060, 25,"Consumer Discretionary",    2550,"Retailing",                                     255040,"Specialty Retail",                              25504060,"Homefurnishing Retail",                         "Owners and operators of furniture and home furnishings retail stores.  Includes residential furniture, homefurnishings, housewares, and interior design.  Excludes home and garden improvement stores, classified in the Home Improvement Retail Sub-Industry." ),
( 30101010, 30,"Consumer Staples",          3010,"Food & Staples Retailing",                      301010,"Food & Staples Retailing",                      30101010,"Drug Retail",                                   "Owners and operators of primarily drug retail stores and pharmacies." ),
( 30101020, 30,"Consumer Staples",          3010,"Food & Staples Retailing",                      301010,"Food & Staples Retailing",                      30101020,"Food Distributors",                             "Distributors of food products to other companies and not directly to the consumer." ),
( 30101030, 30,"Consumer Staples",          3010,"Food & Staples Retailing",                      301010,"Food & Staples Retailing",                      30101030,"Food Retail",                                   "Owners and operators of primarily food retail stores." ),
( 30101040, 30,"Consumer Staples",          3010,"Food & Staples Retailing",                      301010,"Food & Staples Retailing",                      30101040,"Hypermarkets & Super Centers",                  "Owners and operators of hypermarkets and super centers selling food and a wide-range of consumer staple products.  Excludes Food and Drug Retailers classified in the Food Retail and Drug Retail Sub-Industries, respectively." ),
( 30201010, 30,"Consumer Staples",          3020,"Food, Beverage & Tobacco",                      302010,"Beverages",                                     30201010,"Brewers",                                       "Producers of beer and malt liquors. Includes breweries not classified in the Restaurants Sub-Industry." ),
( 30201020, 30,"Consumer Staples",          3020,"Food, Beverage & Tobacco",                      302010,"Beverages",                                     30201020,"Distillers & Vintners",                         "Distillers, vintners and producers of alcoholic beverages not classified in the Brewers Sub-Industry." ),
( 30201030, 30,"Consumer Staples",          3020,"Food, Beverage & Tobacco",                      302010,"Beverages",                                     30201030,"Soft Drinks",                                   "Producers of non-alcoholic beverages including mineral waters. Excludes producers of milk classified in the Packaged Foods Sub-Industry." ),
( 30202010, 30,"Consumer Staples",          3020,"Food, Beverage & Tobacco",                      302020,"Food Products",                                 30202010,"Agricultural Products",                         "Producers of agricultural products. Includes crop growers, owners of plantations and companies that produce and process foods but do not package and market them. Excludes companies classified in the Forest Products Sub-Industry and those that package and market the food products classified in the Packaged Foods Sub-Industry." ),
( 30202020, 30,"Consumer Staples",          3020,"Food, Beverage & Tobacco",                      302020,"Food Products",                                 30202020,"Meat, Poultry & Fish",                          "EXP March 28 2002 -- Companies that raise livestock or poultry, fishing companies and other producers of meat, poultry or fish products." ),
( 30202030, 30,"Consumer Staples",          3020,"Food, Beverage & Tobacco",                      302020,"Food Products",                                 30202030,"Packaged Foods & Meats",                        "Producers of packaged foods including dairy products, fruit juices, meats, poultry, fish and pet foods." ),
( 30203010, 30,"Consumer Staples",          3020,"Food, Beverage & Tobacco",                      302030,"Tobacco",                                       30203010,"Tobacco",                                       "Manufacturers of cigarettes and other tobacco products." ),
( 30301010, 30,"Consumer Staples",          3030,"Household & Personal Products",                 303010,"Household Products",                            30301010,"Household Products",                            "Producers of non-durable household products, including detergents, soaps, diapers and other tissue and household paper products not classified in the Paper Products Sub-Industry." ),
( 30302010, 30,"Consumer Staples",          3030,"Household & Personal Products",                 303020,"Personal Products",                             30302010,"Personal Products",                             "Manufacturers of personal and beauty care products, including cosmetics and perfumes." ),
( 35101010, 35,"Health Care",               3510,"Health Care Equipment & Services",              351010,"Health Care Equipment & Supplies",              35101010,"Health Care Equipment",                         "Manufacturers of health care equipment and devices. Includes medical instruments, drug delivery systems, cardiovascular & orthopedic devices, and diagnostic equipment." ),
( 35101020, 35,"Health Care",               3510,"Health Care Equipment & Services",              351010,"Health Care Equipment & Supplies",              35101020,"Health Care Supplies",                          "Manufacturers of health care supplies and medical products not classified elsewhere. Includes eye care products, hospital supplies, and safety needle & syringe devices." ),
( 35102010, 35,"Health Care",               3510,"Health Care Equipment & Services",              351020,"Health Care Providers & Services",              35102010,"Health Care Distributors",                      "Distributors and wholesalers of health care products not classified elsewhere. " ),
( 35102015, 35,"Health Care",               3510,"Health Care Equipment & Services",              351020,"Health Care Providers & Services",              35102015,"Health Care  Services",                         "Providers of patient health care services not classified elsewhere. Includes dialysis centers, lab testing services, and pharmacy management services. Also includes companies providing business support services to health care providers, such as clerical support services, collection agency services, staffing services and outsourced sales & marketing services" ),
( 35102020, 35,"Health Care",               3510,"Health Care Equipment & Services",              351020,"Health Care Providers & Services",              35102020,"Health Care Facilities",                        "Owners and operators of health care facilities, including hospitals, nursing homes, rehabilitation centers and animal hospitals." ),
( 35102030, 35,"Health Care",               3510,"Health Care Equipment & Services",              351020,"Health Care Providers & Services",              35102030,"Managed Health Care",                           "Owners and operators of Health Maintenance Organizations (HMOs) and other managed plans." ),
( 35103010, 35,"Health Care",               3510,"Health Care Equipment & Services",              351030,"Health Care Technology",                        35103010,"Health Care Technology",                        "Companies providing information technology services primarily to health care providers.  Includes companies providing application, systems and/or data processing software, internet-based tools, and IT consulting services to doctors, hospitals or businesses operating primarily in the Health Care Sector" ),
( 35201010, 35,"Health Care",               3520,"Pharmaceuticals, Biotechnology & Life Sciences",352010,"Biotechnology",                                 35201010,"Biotechnology",                                 "Companies primarily engaged in the research, development, manufacturing and/or marketing of products based on genetic analysis and genetic engineering.  Includes companies specializing in protein-based therapeutics to treat human diseases" ),
( 35202010, 35,"Health Care",               3520,"Pharmaceuticals, Biotechnology & Life Sciences",352020,"Pharmaceuticals",                               35202010,"Pharmaceuticals",                               "Companies engaged in the research, development or production of pharmaceuticals. Includes veterinary drugs." ),
( 35203010, 35,"Health Care",               3520,"Pharmaceuticals, Biotechnology & Life Sciences",352030,"Life Sciences Tools & Services",                35203010,"Life Sciences Tools & Services",                "Companies enabling the drug discovery, development and production continuum by providing analytical tools, instruments, consumables & supplies, clinical trial services and contract research services.  Includes firms primarily servicing the pharmaceutical and biotechnology industries." ),
( 40101010, 40,"Financials",                4010,"Banks",                                         401010,"Commercial Banks",                              40101010,"Diversified Banks",                             "Commercial banks whose businesses are derived primarily from commercial lending operations and have significant business activity in retail banking and small and medium corporate lending.  Excludes banks classified in the Regional Banks and Thrifts & Mortgage Finance sub-industries. Also excludes investment banks classified in the Investment Banking & Brokerage Sub-Industry." ),
( 40101015, 40,"Financials",                4010,"Banks",                                         401010,"Commercial Banks",                              40101015,"Regional Banks",                                "Commercial banks whose businesses are derived primarily from commercial lending operations and have significant business activity in retail banking and small and medium corporate lending. Regional banks tend to operate in limited geographic regions. Excludes companies classified in the Diversified Banks and Thrifts & Mortgage Banks sub-industries. Also excludes investment banks classified in the Investment Banking & Brokerage Sub-Industry." ),
( 40102010, 40,"Financials",                4010,"Banks",                                         401020,"Thrifts & Mortgage Finance",                    40102010,"Thrifts & Mortgage Finance",                    "Financial institutions providing mortgage and mortgage related services.  These include financial institutions whose assets are primarily mortgage related, savings & loans, mortgage GSE's (government sponsored enterprises), mortgage lending institutions, building societies and companies providing insurance to mortgage banks." ),
( 40201010, 40,"Financials",                4020,"Diversified Financials",                        402010,"Diversified Financial Services",                40201010,"Consumer Finance",                              "EXP 04/30/2003 -- Providers of consumer finance services, including personal credit, credit cards, lease financing, mortgage lenders, travel-related money services and pawn shops." ),
( 40201020, 40,"Financials",                4020,"Diversified Financials",                        402010,"Diversified Financial Services",                40201020," Other Diversified Financial Services",         "Providers of a diverse range of financial services and/or with some interest in a wide range of financial services including banking, insurance and capital markets, but with no dominant business line." ),
( 40201030, 40,"Financials",                4020,"Diversified Financials",                        402010,"Diversified Financial Services",                40201030,"Multi-Sector Holdings",                         "A company with significantly diversified holdings across three or more sectors, none of which contributes a majority of profit and/or sales. Stakes held are predominantly of a non-controlling nature.  Includes diversified financial companies where stakes held are of a controlling nature. Excludes other diversified companies classified in the Industrials Conglomerates Sub-Industry." ),
( 40201040, 40,"Financials",                4020,"Diversified Financials",                        402010,"Diversified Financial Services",                40201040,"Specialized Finance",                           "Providers of specialized financial services. Includes credit agencies, stock exchanges and specialty boutiques. Companies in this Sub-Industry derive a majority of revenue from one, specialized line of business." ),
( 40202010, 40,"Financials",                4020,"Diversified Financials",                        402020,"Consumer Finance",                              40202010,"Consumer Finance",                              "Providers of consumer finance services, including personal credit, credit cards, lease financing, travel-related money services and pawn shops.  Excludes mortgage lenders classified in the Thrifts & Mortgage Banks Sub-Industry." ),
( 40203010, 40,"Financials",                4020,"Diversified Financials",                        402030,"Capital Markets",                               40203010,"Asset Management & Custody Banks",              "Financial institutions primarily engaged in investment management and/or related custody and securities fee-based services. Includes companies operating mutual funds, closed-end funds and unit investment trusts.  Excludes banks and other financial institutions primarily involved in commercial lending, investment banking, brokerage and other specialized financial activities. " ),
( 40203020, 40,"Financials",                4020,"Diversified Financials",                        402030,"Capital Markets",                               40203020,"Investment Banking & Brokerage",                "Financial institutions primarily engaged in investment banking & brokerage services, including equity and debt underwriting, mergers and acquisitions, securities lending and advisory services. Excludes banks and other financial institutions primarily involved in commercial lending, asset management and specialized financial activities. " ),
( 40203030, 40,"Financials",                4020,"Diversified Financials",                        402030,"Capital Markets",                               40203030,"Diversified Capital Markets",                   "Financial institutions primarily engaged in diversified capital markets activities, including a significant presence in at least two of the following area: large/major corporate lending, investment banking, brokerage and asset management. Excludes less diversified companies classified in the Asset Management & Custody Banks or Investment Banking & Brokerage sub-industries.  Also excludes companies classified in the Banks or Insurance industry groups or the Consumer Finance Sub-Industry. " ),
( 40301010, 40,"Financials",                4030,"Insurance",                                     403010,"Insurance",                                     40301010,"Insurance Brokers",                             "Insurance and reinsurance brokerage firms." ),
( 40301020, 40,"Financials",                4030,"Insurance",                                     403010,"Insurance",                                     40301020,"Life & Health Insurance",                       "Companies providing primarily life, disability, indemnity or supplemental health insurance. Excludes managed care companies classified in the Managed Health Care Sub-Industry." ),
( 40301030, 40,"Financials",                4030,"Insurance",                                     403010,"Insurance",                                     40301030,"Multi-line Insurance",                          "Insurance companies with diversified interests in life, health and property and casualty insurance." ),
( 40301040, 40,"Financials",                4030,"Insurance",                                     403010,"Insurance",                                     40301040,"Property & Casualty Insurance",                 "Companies providing primarily property and casualty insurance." ),
( 40301050, 40,"Financials",                4030,"Insurance",                                     403010,"Insurance",                                     40301050,"Reinsurance",                                   "Companies providing primarily reinsurance." ),
( 40401010, 40,"Financials",                4040,"Real Estate",                                   404010,"Real Estate",                                   40401010,"Real Estate Investment Trusts",                 "EXP 04/28/2006 -- Real estate investment trusts (REITs).  Includes Property Trusts." ),
( 40401020, 40,"Financials",                4040,"Real Estate",                                   404010,"Real Estate",                                   40401020,"Real Estate Management & Development",          "EXP 04/28/2006 -- Companies engaged in real estate ownership, development or  management." ),
( 40402010, 40,"Financials",                4040,"Real Estate",                                   404020,"Real Estate Investment Trusts (REITs)",         40402010,"Diversified REIT's",                            "A company or Trust with significantly diversified operations across two or more property types." ),
( 40402020, 40,"Financials",                4040,"Real Estate",                                   404020,"Real Estate Investment Trusts (REITs)",         40402020,"Industrial REIT's",                             "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of industrial properties. Includes companies operating industrial warehouses and distribution properties." ),
( 40402030, 40,"Financials",                4040,"Real Estate",                                   404020,"Real Estate Investment Trusts (REITs)",         40402030,"Mortgage REIT's",                               "Companies or Trusts that service, originate, purchase and/or securitize residential and/or commercial mortgage loans.  Includes trusts that invest in mortgage-backed securities and other mortgage related assets." ),
( 40402040, 40,"Financials",                4040,"Real Estate",                                   404020,"Real Estate Investment Trusts (REITs)",         40402040,"Office REIT's",                                 "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of office properties." ),
( 40402050, 40,"Financials",                4040,"Real Estate",                                   404020,"Real Estate Investment Trusts (REITs)",         40402050,"Residential REIT's",                            "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of residential properties including multifamily homes, apartments, manufactured homes and student housing properties" ),
( 40402060, 40,"Financials",                4040,"Real Estate",                                   404020,"Real Estate Investment Trusts (REITs)",         40402060,"Retail REIT's",                                 "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of shopping malls, outlet malls, neighborhood and community shopping centers." ),
( 40402070, 40,"Financials",                4040,"Real Estate",                                   404020,"Real Estate Investment Trusts (REITs)",         40402070,"Specialized REIT's",                            "Companies or Trusts engaged in the acquisition, development, ownership, leasing, management and operation of properties not classified elsewhere. Includes trusts that operate and invest in health care, leisure, hotel/resort and storage properties. It also includes REITs that do not generate a majority of their revenues and income from real estate rental and leasing operations." ),
( 40403010, 40,"Financials",                4040,"Real Estate",                                   404030,"Real Estate Management & Development",          40403010,"Diversified Real Estate Activities",            "Companies engaged in a diverse spectrum of real estate activities including real estate development & sales, real estate management, or real estate services, but with no dominant business line." ),
( 40403020, 40,"Financials",                4040,"Real Estate",                                   404030,"Real Estate Management & Development",          40403020,"Real Estate Operating Companies",               "Companies engaged in operating real estate properties for the purpose of leasing & management." ),
( 40403030, 40,"Financials",                4040,"Real Estate",                                   404030,"Real Estate Management & Development",          40403030,"Real Estate Development",                       "Companies that develop real estate and sell the properties after development. Excludes companies classified in the Homebuilding Sub-Industry." ),
( 40403040, 40,"Financials",                4040,"Real Estate",                                   404030,"Real Estate Management & Development",          40403040,"Real Estate Services",                          "Real estate service providers such as real estate agents, brokers & real estate appraisers." ),
( 45101010, 45,"Information Technology",    4510,"Software & Services",                           451010,"Internet Software & Services",                  45101010,"Internet Software & Services",                  "Companies developing and marketing internet software and/or providing internet services including online databases and interactive services, web address registration services, database construction and internet design services. Excludes companies classified in the Internet Retail Sub-Industry." ),
( 45102010, 45,"Information Technology",    4510,"Software & Services",                           451020,"IT Services",                                   45102010,"IT Consulting & Other Services",                "Providers of information technology and systems integration services not classified in the Data Processing & Outsourced Services or Internet Software & Services Sub-Industries.  Includes information technology consulting and information management services." ),
( 45102020, 45,"Information Technology",    4510,"Software & Services",                           451020,"IT Services",                                   45102020,"Data Processing & Outsourced Services",         "Providers of commercial electronic data processing and/or business process outsourcing services.  Includes companies that provide services for back-office automation." ),
( 45103010, 45,"Information Technology",    4510,"Software & Services",                           451030,"Software",                                      45103010,"Application Software",                          "Companies engaged in developing and producing software designed for specialized applications for the business or consumer market. Includes enterprise and technical software. Excludes companies classified in the Home Entertainment Software Sub-Industry. Also excludes companies producing systems or database management software classified in the Systems Software Sub-Industry." ),
( 45103020, 45,"Information Technology",    4510,"Software & Services",                           451030,"Software",                                      45103020,"Systems Software",                              "Companies engaged in developing and producing systems and database management software." ),
( 45103030, 45,"Information Technology",    4510,"Software & Services",                           451030,"Software",                                      45103030,"Home Entertainment Software",                   "Manufacturers of home entertainment software and educational software used primarily in the home." ),
( 45201020, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452010,"Communications Equipment",                      45201020,"Communications Equipment",                      "Manufacturers of communication equipment and products, including LANs, WANs, routers, telephones, switchboards and exchanges." ),
( 45201010, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452010,"Communications Equipment",                      45201010,"Networking Equipment",                          "EXP 04/30/2003. -- Manufacturers of computer networking equipment and products, including LANs, WANs and routers." ),
( 45201020, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452010,"Communications Equipment",                      45201020,"Telecommunications Equipment",                  "EXP 04/30/2003. -- Manufacturers of telecommunication equipment, including telephones, switchboards and exchanges. Excludes companies classified in the Networking Equipment Sub-Industry." ),
( 45202010, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452020,"Computers & Peripherals",                       45202010,"Computer Hardware",                             "Manufacturers of personal computers, servers, mainframes and workstations. Includes manufacturers of Automatic Teller Machines (ATMs). Excludes manufacturers of copiers, faxes and related products classified in the Office Electronics Sub-Industry." ),
( 45202020, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452020,"Computers & Peripherals",                       45202020,"Computer Storage & Peripherals",                "Manufacturers of electronic computer components and peripherals. Includes data storage components, motherboards, audio and video cards, monitors, keyboards, printers and other peripherals. Excludes semiconductors classified in the Semiconductors Sub-Industry." ),
( 45203010, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452030,"Electronic Equipment, Instruments & Components",45203010,"Electronic Equipment & Instruments ",           "Manufacturers of electronic equipment and instruments including analytical, electronic test and measurement instruments, scanner/barcode products, lasers, display screens, point-of-sales machines, and security system equipment." ),
( 45203015, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452030,"Electronic Equipment, Instruments & Components",45203015,"Electronic Components",                         "Manufacturers of electronic components. Includes electronic components, connection devices, electron tubes, electronic capacitors and resistors, electronic coil, printed circuit board, transformer and other inductors, signal processing technology/components." ),
( 45203020, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452030,"Electronic Equipment, Instruments & Components",45203020,"Electronic Manufacturing Services",             "Producers of electronic equipment mainly for the OEM (Original Equipment Manufacturers) markets." ),
( 45203030, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452030,"Electronic Equipment, Instruments & Components",45203030,"Technology Distributors",                       "Distributors of technology hardware and equipment. Includes distributors of communications equipment, computers & peripherals, semiconductors, and electronic equipment and components." ),
( 45204010, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452040,"Office Electronics",                            45204010,"Office Electronics",                            "Manufacturers of office electronic equipment including copiers and faxes." ),
( 45205010, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452050,"Semiconductor Equipment & Products",            45205010,"Semiconductor Equipment",                       "EXP 04/30/2003. -- Manufacturers of semiconductor equipment." ),
( 45205020, 45,"Information Technology",    4520,"Technology Hardware & Equipment",               452050,"Semiconductor Equipment & Products",            45205020,"Semiconductors",                                "EXP 04/30/2003. -- Manufacturers of semiconductors and related products." ),
( 45301010, 45,"Information Technology",    4530,"Semiconductors & Semiconductor Equipment",      453010,"Semiconductors & Semiconductor Equipment",      45301010,"Semiconductor Equipment",                       "Manufacturers of semiconductor equipment, including manufacturers of the raw material and equipment used in the solar power industry." ),
( 45301020, 45,"Information Technology",    4530,"Semiconductors & Semiconductor Equipment",      453010,"Semiconductors & Semiconductor Equipment",      45301020,"Semiconductors",                                "Manufacturers of semiconductors and related products, including manufacturers of solar modules and cells." ),
( 50101010, 50,"Telecommunication Services",5010,"Telecommunication Services",                    501010,"Diversified Telecommunication Services",        50101010,"Alternative Carriers",                          "Providers of communications and high-density data transmission services primarily through a high bandwidth/fiber-optic cable network." ),
( 50101020, 50,"Telecommunication Services",5010,"Telecommunication Services",                    501010,"Diversified Telecommunication Services",        50101020,"Integrated Telecommunication Services",         "Operators of primarily fixed-line telecommunications networks and companies providing both wireless and fixed-line telecommunications services not classified elsewhere." ),
( 50102010, 50,"Telecommunication Services",5010,"Telecommunication Services",                    501020,"Wireless Telecommunication Services",           50102010,"Wireless Telecommunication Services",           "Providers of primarily cellular or wireless telecommunication services, including paging services." ),
( 55101010, 55,"Utilities",                 5510,"Utilities",                                     551010,"Electric Utilities",                            55101010,"Electric Utilities",                            "Companies that produce or distribute electricity. Includes both nuclear and non-nuclear facilities." ),
( 55102010, 55,"Utilities",                 5510,"Utilities",                                     551020,"Gas Utilities",                                 55102010,"Gas Utilities",                                 "Companies whose main charter is to distribute and transmit natural and manufactured gas. Excludes companies primarily involved in gas exploration or production classified in the Oil & Gas Exploration & Production Sub-Industry.   Also excludes diversified midstream natural gas companies classified in the Oil & Gas Storage & Transportation Sub-Industry." ),
( 55103010, 55,"Utilities",                 5510,"Utilities",                                     551030,"Multi-Utilities",                               55103010,"Multi-Utilities",                               "Utility companies with significantly diversified activities in addition to core Electric Utility, Gas Utility and/or Water Utility operations." ),
( 55104010, 55,"Utilities",                 5510,"Utilities",                                     551040,"Water Utilities",                               55104010,"Water Utilities",                               "Companies that purchase and redistribute water to the end-consumer. Includes large-scale water treatment systems." ),
( 55105010, 55,"Utilities",                 5510,"Utilities",                                     551050,"Independent Power Producers & Energy Traders",  55105010,"Independent Power Producers & Energy Traders",  "Companies that operate as Independent Power Producers (IPPs), Gas & Power Marketing & Trading Specialists and/or Integrated Energy Merchants. Includes producers of solar power and wind power, used to generate electricity. Also includes companies that generate electricity and/or power through alternative energy sources such as biogas, biomass, clean energy, geothermal, waste, water and waves.  Excludes electric transmission companies and utility distribution companies classified in the Electric Utilities Sub-Industry." )
)
}

object LinkedInIndustry extends RamEntity( tid = "a0Ow" ) {
  override lazy val dbName = "businessCategories"

  "id"              is DbLong              ;

  "group"           is DbChar(50)          ;
  "description"     is DbChar(400)         ;

  static(
( "id", "group",           "description" ),
( 47,    "corp fin",       "Accounting" ),
( 94,    "man tech tran",  20302010, "Airlines/Aviation" ),
( 120,   "leg org",        "Alternative Dispute Resolution" ),
( 125,   "hlth",           "Alternative Medicine" ),
( 127,   "art med",        "Animation" ),
( 19,    "good",           "Apparel & Fashion" ),
( 50,    "cons",           "Architecture & Planning" ),
( 111,   "art med rec",    "Arts and Crafts" ),
( 53,    "man",            "Automotive" ),
( 52,    "gov man",        "Aviation & Aerospace" ),
( 41,    "fin",            "Banking" ),
( 12,    "gov hlth tech",  "Biotechnology" ),
( 36,    "med rec",        "Broadcast Media" ),
( 49,    "cons",           "Building Materials" ),
( 138,   "corp man",       "Business Supplies and Equipment" ),
( 129,   "fin",            "Capital Markets" ),
( 54,    "man",            "Chemicals" ),
( 90,    "org serv",       "Civic & Social Organization" ),
( 51,    "cons gov",       "Civil Engineering" ),
( 128,   "cons corp fin",  "Commercial Real Estate" ),
( 118,   "tech",           "Computer & Network Security" ),
( 109,   "med rec",        "Computer Games" ),
( 3,     "tech",           "Computer Hardware" ),
( 5,     "tech",           "Computer Networking" ),
( 6,     "tech",           "Internet" ),
( 4,     "tech",           "Computer Software" ),
( 48,    "cons",           "Construction" ),
( 24,    "good man",       "Consumer Electronics" ),
( 25,    "good man",       "Consumer Goods" ),
( 91,    "org serv",       "Consumer Services" ),
( 18,    "good",           "Cosmetics" ),
( 65,    "agr",            "Dairy" ),
( 1,     "gov tech",       "Defense & Space" ),
( 99,    "art med",        "Design" ),
( 69,    "edu",            "Education Management" ),
( 132,   "edu org",        "E-Learning" ),
( 112,   "good man",       "Electrical/Electronic Manufacturing" ),
( 28,    "med rec",        "Entertainment" ),
( 86,    "org serv",       "Environmental Services" ),
( 110,   "corp rec serv",  "Events Services" ),
( 76,    "gov",            "Executive Office" ),
( 122,   "corp serv",      "Facilities Services" ),
( 63,    "agr",            "Farming" ),
( 43,    "fin",            "Financial Services" ),
( 38,    "art med rec",    "Fine Art" ),
( 66,    "agr",            "Fishery" ),
( 34,    "rec serv",       "Food & Beverages" ),
( 23,    "good man serv",  "Food Production" ),
( 101,   "org",            "Fund-Raising" ),
( 26,    "good man",       "Furniture" ),
( 29,    "rec",            "Gambling & Casinos" ),
( 145,   "cons man",       "Glass, Ceramics & Concrete" ),
( 75,    "gov",            "Government Administration" ),
( 148,   "gov",            "Government Relations" ),
( 140,   "art med",        "Graphic Design" ),
( 124,   "hlth rec",       "Health, Wellness and Fitness" ),
( 68,    "edu",            "Higher Education" ),
( 14,    "hlth",           "Hospital & Health Care" ),
( 31,    "rec serv tran",  "Hospitality" ),
( 137,   "corp",           "Human Resources" ),
( 134,   "corp good tran", "Import and Export" ),
( 88,    "org serv",       "Individual & Family Services" ),
( 147,   "cons man",       "Industrial Automation" ),
( 84,    "med serv",       "Information Services" ),
( 96,    "tech",           "Information Technology and Services" ),
( 42,    "fin",            "Insurance" ),
( 74,    "gov",            "International Affairs" ),
( 141,   "gov org tran",   "International Trade and Development" ),
( 45,    "fin",            "Investment Banking" ),
( 46,    "fin",            "Investment Management" ),
( 73,    "gov leg",        "Judiciary" ),
( 77,    "gov leg",        "Law Enforcement" ),
( 9,     "leg",            "Law Practice" ),
( 10,    "leg",            "Legal Services" ),
( 72,    "gov leg",        "Legislative Office" ),
( 30,    "rec serv tran",  "Leisure, Travel & Tourism" ),
( 85,    "med rec serv",   "Libraries" ),
( 116,   "corp tran",      "Logistics and Supply Chain" ),
( 143,   "good",           "Luxury Goods & Jewelry" ),
( 55,    "man",            "Machinery" ),
( 11,    "corp",           "Management Consulting" ),
( 95,    "tran",           "Maritime" ),
( 97,    "corp",           "Market Research" ),
( 80,    "corp med",       "Marketing and Advertising" ),
( 135,   "cons gov man",   "Mechanical or Industrial Engineering" ),
( 126,   "med rec",        "Media Production" ),
( 17,    "hlth",           "Medical Devices" ),
( 13,    "hlth",           "Medical Practice" ),
( 139,   "hlth",           "Mental Health Care" ),
( 71,    "gov",            "Military" ),
( 56,    "man",            "Mining & Metals" ),
( 35,    "art med rec",    "Motion Pictures and Film" ),
( 37,    "art med rec",    "Museums and Institutions" ),
( 115,   "art rec",        "Music" ),
( 114,   "gov man tech",   "Nanotechnology" ),
( 81,    "med rec",        "Newspapers" ),
( 100,   "org",            "Non-Profit Organization Management" ),
( 57,    "man",            "Oil & Energy" ),
( 113,   "med",            "Online Media" ),
( 123,   "corp",           "Outsourcing/Offshoring" ),
( 87,    "serv tran",      "Package/Freight Delivery" ),
( 146,   "good man",       "Packaging and Containers" ),
( 61,    "man",            "Paper & Forest Products" ),
( 39,    "art med rec",    "Performing Arts" ),
( 15,    "hlth tech",      "Pharmaceuticals" ),
( 131,   "org",            "Philanthropy" ),
( 136,   "art med rec",    "Photography" ),
( 117,   "man",            "Plastics" ),
( 107,   "gov org",        "Political Organization" ),
( 67,    "edu",            "Primary/Secondary Education" ),
( 83,    "med rec",        "Printing" ),
( 105,   "corp",           "Professional Training & Coaching" ),
( 102,   "corp org",       "Program Development" ),
( 79,    "gov",            "Public Policy" ),
( 98,    "corp",           "Public Relations and Communications" ),
( 78,    "gov",            "Public Safety" ),
( 82,    "med rec",        "Publishing" ),
( 62,    "man",            "Railroad Manufacture" ),
( 64,    "agr",            "Ranching" ),
( 44,    "cons fin good",  "Real Estate" ),
( 40,    "rec serv",       "Recreational Facilities and Services" ),
( 89,    "org serv",       "Religious Institutions" ),
( 144,   "gov man org",    "Renewables & Environment" ),
( 70,    "edu gov",        "Research" ),
( 32,    "rec serv",       "Restaurants" ),
( 27,    "good man",       "Retail" ),
( 121,   "corp org serv",  "Security and Investigations" ),
( 7,     "tech",           "Semiconductors" ),
( 58,    "man",            "Shipbuilding" ),
( 20,    "good rec",       "Sporting Goods" ),
( 33,    "rec",            "Sports" ),
( 104,   "corp",           "Staffing and Recruiting" ),
( 22,    "good",           "Supermarkets" ),
( 8,     "gov tech",       "Telecommunications" ),
( 60,    "man",            "Textiles" ),
( 130,   "gov org",        "Think Tanks" ),
( 21,    "good",           "Tobacco" ),
( 108,   "corp gov serv",  "Translation and Localization" ),
( 92,    "tran",           "Transportation/Trucking/Railroad" ),
( 59,    "man",            "Utilities" ),
( 106,   "fin tech",       "Venture Capital & Private Equity" ),
( 16,    "hlth",           "Veterinary" ),
( 93,    "tran",           "Warehousing" ),
( 133,   "good",           "Wholesale" ),
( 142,   "good man rec",   "Wine and Spirits" ),
( 119,   "tech",           "Wireless" ),
( 103,   "art med rec",    "Writing and Editing" ) )
}
