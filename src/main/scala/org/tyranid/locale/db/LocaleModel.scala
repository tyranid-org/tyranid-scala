/**
 * Copyright (c) 2008-2011 Tyranid <http://tyranid.org>
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

package org.tyranid.locale.db

import org.tyranid.Imp.symbol
import org.tyranid.db.{ DbInt, DbChar }
import org.tyranid.db.ram.RamEntity


object Region extends RamEntity( tid = "a01t" ) {
  "id"     is DbInt      is 'key;
  "name"   is DbChar(64) is 'label;
  "code"   is DbChar(4)  ; 

	static(
	( "id",  "name",				         "code" ),
	(  1,    "Alabama",              "AL" ),
	(  2,    "Alaska",               "AK" ),
	(  3,    "Arizona",              "AZ" ),
	(  4,    "Arkansas",             "AR" ),
	(  5,    "California",           "CA" ),
	(  6,    "Colorado",             "CO" ),
	(  7,    "Connecticut",          "CT" ),
	(  8,    "Delaware",             "DE" ),
	(  9,    "Florida",              "FL" ),
	(  10,   "Georgia",              "GA" ),
	(  11,   "Hawaii",               "HI" ),
	(  12,   "Idaho",                "ID" ),
	(  13,   "Illinois",             "IL" ),
	(  14,   "Indiana",              "IN" ),
	(  15,   "Iowa",                 "IA" ),
	(  16,   "Kansas",               "KS" ),
	(  17,   "Kentucky",             "KY" ),
	(  18,   "Louisiana",            "LA" ),
	(  19,   "Maine",                "ME" ),
	(  20,   "Maryland",             "MD" ),
	(  21,   "Massachusetts",        "MA" ),
	(  22,   "Michigan",             "MI" ),
	(  23,   "Minnesota",            "MN" ),
	(  24,   "Mississippi",          "MS" ),
	(  25,   "Missouri",             "MO" ),
	(  26,   "Montana",              "MT" ),
	(  27,   "Nebraska",             "NE" ),
	(  28,   "Nevada",               "NV" ),
	(  29,   "New Hampshire",        "NH" ),
	(  30,   "New Jersey",           "NJ" ),
	(  31,   "New Mexico",           "NM" ),
	(  32,   "New York",             "NY" ),
	(  33,   "North Carolina",       "NC" ),
	(  34,   "North Dakota",         "ND" ),
	(  35,   "Ohio",                 "OH" ),
	(  36,   "Oklahoma",             "OK" ),
	(  37,   "Oregon",               "OR" ),
	(  38,   "Pennsylvania",         "PA" ),
	(  39,   "Rhode Island",         "RI" ),
	(  40,   "South Carolina",       "SC" ),
	(  41,   "South Dakota",         "SD" ),
	(  42,   "Tennessee",            "TN" ),
	(  43,   "Texas",                "TX" ),
	(  44,   "Utah",                 "UT" ),
	(  45,   "Vermont",              "VT" ),
	(  46,   "Virginia",             "VA" ),
	(  47,   "Washington",           "WA" ),
	(  48,   "West Virginia",        "WV" ),
	(  49,   "Wisconsin",            "WI" ),
	(  50,   "Wyoming",              "WY" ),
	(  3874, "District of Columbia", "DC" ) )	    
}

object Country extends RamEntity( tid = "a02t" ) {
  "id"     is DbInt      is 'key;
  "name"   is DbChar(64) is 'label;
  "code"   is DbChar(4)  ; 

	static(
       ( "id", "name",				                               "code" ),
	(    4077, "Andorra",                                      "AN" ),
	(    4078, "United Arab Emirates",                         "AE" ),
	(    4079, "Afghanistan",                                  "AF" ),
	(    4080, "Antigua and Barbuda",                          "AC" ),
	(    4081, "Anguilla",                                     "AV" ),
	(    4082, "Albania",                                      "AL" ),
	(    4083, "Armenia",                                      "AM" ),
	(    4084, "Netherlands Antilles",                         "NT" ),
	(    4085, "Angola",                                       "AO" ),
	(    4088, "Argentina",                                    "AR" ),
	(    4089, "American Samoa",                               "AQ" ),
	(    4090, "Austria",                                      "AU" ),
	(    4091, "Australia",                                    "AS" ),
	(    4092, "Aruba",                                        "AA" ),
	(    4093, "Aland Islands",                                ""   ),
	(    4094, "Azerbaijan",                                   "AJ" ),
	(    4095, "Bosnia and Herzegovina",                       "BK" ),
	(    4096, "Barbados",                                     "BB" ),
	(    4097, "Bangladesh",                                   "BG" ),
	(    4098, "Belgium",                                      "BE" ),
	(    4099, "Burkina Faso",                                 "UV" ),
	(    4100, "Bulgaria",                                     "BU" ),
	(    4101, "Bahrain",                                      "BA" ),
	(    4102, "Burundi",                                      "BY" ),
	(    4103, "Benin",                                        "BN" ),
	(    4104, "Bermuda",                                      "BD" ),
	(    4105, "Brunei Darussalam",                            "BX" ),
	(    4106, "Bolivia",                                      "BL" ),
	(    4107, "Brazil",                                       "BR" ),
	(    4108, "Bahamas",                                      "BF" ),
	(    4109, "Bhutan",                                       "BT" ),
	(    4110, "Bouvet Island",                                "BV" ),
	(    4111, "Botswana",                                     "BC" ),
	(    4112, "Belarus",                                      "BO" ),
	(    4113, "Belize",                                       "BH" ),
	(    4114, "Canada",                                       "CA" ),
	(    4115, "Cocos (Keeling) Islands",                      "CK" ),
	(    4116, "Congo, Democratic Republic of the",            "CG" ),
	(    4326, "Congo (Zaire)",                                "CG" ),
	(    4117, "Central African Republic",                     "CT" ),
	(    4118, "Congo",                                        "CF" ),
	(    4119, "Switzerland",                                  "SZ" ),
	(    4120, "Cote d'Ivoire",                                "IV" ),
	(    4121, "Cook Islands",                                 "CW" ),
	(    4122, "Chile",                                        "CI" ),
	(    4123, "Cameroon",                                     "CM" ),
	(    4124, "China",                                        "CH" ),
	(    4125, "Colombia",                                     "CO" ),
	(    4126, "Costa Rica",                                   "CS" ),
	(    4127, "Cuba",                                         "CU" ),
	(    4128, "Cape Verde",                                   "CV" ),
	(    4129, "Christmas Island",                             "KT" ),
	(    4130, "Cyprus",                                       "CY" ),
	(    4131, "Czech Republic",                               "EZ" ),
	(    4132, "Germany",                                      "GM" ),
	(    4133, "Djibouti",                                     "DJ" ),
	(    4134, "Denmark",                                      "DA" ),
	(    4135, "Dominica",                                     "DO" ),
	(    4136, "Dominican Republic",                           "DR" ),
	(    4137, "Algeria",                                      "AG" ),
	(    4138, "Ecuador",                                      "EC" ),
	(    4139, "Estonia",                                      "EN" ),
	(    4140, "Egypt",                                        "EG" ),
	(    4141, "Western Sahara",                               "WI" ),
	(    4142, "Eritrea",                                      "ER" ),
	(    4143, "Spain",                                        "SP" ),
	(    4144, "Ethiopia",                                     "ET" ),
	(    4146, "Finland",                                      "FI" ),
	(    4147, "Fiji",                                         "FJ" ),
	(    4148, "Falkland Islands (Malvinas)",                  "FK" ),
	(    4149, "Micronesia, Federated States of",              "FM" ),
	(    4150, "Faroe Islands",                                "FO" ),
	(    4151, "France",                                       "FR" ),
	(    4152, "Gabon",                                        "GB" ),
	(    4153, "United Kingdom",                               "UK" ),
	(    4154, "Grenada",                                      "GJ" ),
	(    4155, "Georgia",                                      "GG" ),
	(    4156, "French Guiana",                                "FG" ),
	(    4157, "Guernsey",                                     "GK" ),
	(    4158, "Ghana",                                        "GH" ),
	(    4159, "Gibraltar",                                    "GI" ),
	(    4160, "Greenland",                                    "GL" ),
	(    4161, "Gambia",                                       "GA" ),
	(    4162, "Guinea",                                       "GV" ),
	(    4163, "Guadeloupe",                                   "GP" ),
	(    4164, "Equatorial Guinea",                            "EK" ),
	(    4165, "Greece",                                       "GR" ),
	(    4166, "South Georgia and the South Sandwich Islands", "SX" ),
	(    4167, "Guatemala",                                    "GT" ),
	(    4168, "Guam",                                         "GQ" ),
	(    4169, "Guinea-Bissau",                                "PU" ),
	(    4170, "Guyana",                                       "GY" ),
	(    4172, "Heard Island and McDonald Islands",            "HM" ),
	(    4173, "Honduras",                                     "HO" ),
	(    4174, "Croatia",                                      "HR" ),
	(    4175, "Haiti",                                        "HA" ),
	(    4176, "Hungary",                                      "HU" ),
	(    4177, "Indonesia",                                    "ID" ),
	(    4178, "Ireland",                                      "EI" ),
	(    4179, "Israel",                                       "IS" ),
	(    4180, "Isle of Man",                                  "IM" ),
	(    4181, "India",                                        "IN" ),
	(    4182, "British Indian Ocean Territory",               "IO" ),
	(    4183, "Iraq",                                         "IZ" ),
	(    4184, "Iran, Islamic Republic of",                    "IR" ),
	(    4185, "Iceland",                                      "IC" ),
	(    4186, "Italy",                                        "IT" ),
	(    4187, "Jersey",                                       "JE" ),
	(    4188, "Jamaica",                                      "JM" ),
	(    4189, "Jordan",                                       "JO" ),
	(    4190, "Japan",                                        "JA" ),
	(    4191, "Kenya",                                        "KE" ),
	(    4192, "Kyrgyzstan",                                   "KG" ),
	(    4193, "Cambodia",                                     "CB" ),
	(    4194, "Kiribati",                                     "KR" ),
	(    4195, "Comoros",                                      "CN" ),
	(    4196, "Saint Kitts and Nevis",                        "SC" ),
	(    4197, "North Korea",                                  "KN" ),
	(    4198, "South Korea",                                  "KS" ),
	(    4199, "Kuwait",                                       "KU" ),
	(    4200, "Cayman Islands",                               "CJ" ),
	(    4201, "Kazakhstan",                                   "KZ" ),
	(    4202, "Lao People's Democratic Republic",             "LA" ),
	(    4203, "Lebanon",                                      "LE" ),
	(    4204, "Saint Lucia",                                  "ST" ),
	(    4205, "Liechtenstein",                                "LS" ),
	(    4206, "Sri Lanka",                                    "CE" ),
	(    4207, "Liberia",                                      "LI" ),
	(    4208, "Lesotho",                                      "LT" ),
	(    4209, "Lithuania",                                    "LH" ),
	(    4210, "Luxembourg",                                   "LU" ),
	(    4211, "Latvia",                                       "LG" ),
	(    4212, "Libyan Arab Jamahiriya",                       ""   ),
	(    4213, "Morocco",                                      "MO" ),
	(    4214, "Monaco",                                       "MN" ),
	(    4215, "Moldova, Republic of",                         "MD" ),
	(    4216, "Montenegro",                                   "MW" ),
	(    4217, "Madagascar",                                   "MA" ),
	(    4218, "Marshall Islands",                             "RM" ),
	(    4219, "Macedonia",                                    "MK" ),
	(    4220, "Mali",                                         "ML" ),
	(    4221, "Myanmar",                                      "BM" ),
	(    4222, "Mongolia",                                     "MG" ),
	(    4223, "Macao",                                        "MC" ),
	(    4224, "Northern Mariana Islands",                     "CQ" ),
	(    4225, "Martinique",                                   "MB" ),
	(    4226, "Mauritania",                                   "MR" ),
	(    4227, "Montserrat",                                   "MH" ),
	(    4228, "Malta",                                        "MT" ),
	(    4229, "Mauritius",                                    "MP" ),
	(    4230, "Maldives",                                     "MV" ),
	(    4231, "Malawi",                                       "MI" ),
	(    4232, "Mexico",                                       "MX" ),
	(    4233, "Malaysia",                                     "MY" ),
	(    4234, "Mozambique",                                   "MZ" ),
	(    4235, "Namibia",                                      "WA" ),
	(    4236, "New Caledonia",                                "NC" ),
	(    4237, "Niger",                                        "NG" ),
	(    4238, "Norfolk Island",                               "NF" ),
	(    4239, "Nigeria",                                      "NI" ),
	(    4240, "Nicaragua",                                    "NU" ),
	(    4241, "Netherlands",                                  "NL" ),
	(    4242, "Norway",                                       "NO" ),
	(    4243, "Nepal",                                        "NP" ),
	(    4244, "Nauru",                                        "NR" ),
	(    4245, "Niue",                                         "NE" ),
	(    4246, "New Zealand",                                  "NZ" ),
	(    4247, "Oman",                                         "MU" ),
	(    4248, "Panama",                                       "PM" ),
	(    4249, "Peru",                                         "PE" ),
	(    4250, "French Polynesia",                             "FP" ),
	(    4251, "Papua New Guinea",                             "PP" ),
	(    4252, "Philippines",                                  "RP" ),
	(    4253, "Pakistan",                                     "PK" ),
	(    4254, "Poland",                                       "PL" ),
	(    4255, "Saint Pierre and Miquelon",                    "SB" ),
	(    4256, "Pitcairn",                                     "PC" ),
	(    4257, "Puerto Rico",                                  "RQ" ),
	(    4258, "Palestinian Territory",                        ""   ),
	(    4259, "Portugal",                                     "PO" ),
	(    4260, "Palau",                                        "PS" ),
	(    4261, "Paraguay",                                     "PA" ),
	(    4262, "Qatar",                                        "QA" ),
	(    4263, "Reunion",                                      "RE" ),
	(    4264, "Romania",                                      "RO" ),
	(    4265, "Serbia",                                       "RI" ),
	(    4266, "Russian Federation",                           "RS" ),
	(    4267, "Rwanda",                                       "RW" ),
	(    4268, "Saudi Arabia",                                 "SA" ),
	(    4269, "Solomon Islands",                              "BP" ),
	(    4270, "Seychelles",                                   "SE" ),
	(    4271, "Sudan",                                        "SU" ),
	(    4272, "Sweden",                                       "SW" ),
	(    4273, "Singapore",                                    "SN" ),
	(    4274, "Saint Helena",                                 "SH" ),
	(    4275, "Slovenia",                                     "SI" ),
	(    4276, "Svalbard and Jan Mayen",                       "SV" ),
	(    4277, "Slovakia",                                     "LO" ),
	(    4278, "Sierra Leone",                                 "SL" ),
	(    4279, "San Marino",                                   "SM" ),
	(    4280, "Senegal",                                      "SG" ),
	(    4281, "Somalia",                                      "SO" ),
	(    4282, "Suriname",                                     "NS" ),
	(    4283, "Sao Tome and Principe",                        "TP" ),
	(    4284, "El Salvador",                                  "ES" ),
	(    4285, "Syria",                                        "SY" ),
	(    4286, "Swaziland",                                    "WZ" ),
	(    4287, "Turks and Caicos Islands",                     "TK" ),
	(    4288, "Chad",                                         "CD" ),
	(    4289, "French Southern Territories",                  "FS" ),
	(    4290, "Togo",                                         "TO" ),
	(    4291, "Thailand",                                     "TH" ),
	(    4292, "Tajikistan",                                   "TI" ),
	(    4293, "Tokelau",                                      "TL" ),
	(    4294, "Timor-Leste",                                  "TT" ),
	(    4295, "Turkmenistan",                                 "TX" ),
	(    4296, "Tunisia",                                      "TS" ),
	(    4297, "Tonga",                                        "TN" ),
	(    4298, "Turkey",                                       "TU" ),
	(    4299, "Trinidad and Tobago",                          "TD" ),
	(    4300, "Tuvalu",                                       "TV" ),
	(    4301, "Taiwan",                                       "TW" ),
	(    4302, "Tanzania, United Republic of",                 "TZ" ),
	(    4303, "Ukraine",                                      "UP" ),
	(    4304, "Uganda",                                       "UG" ),
	(    4305, "United States Minor Outlying Islands",         ""   ),
	(    4306, "United States",                                "US" ),
	(    4307, "Uruguay",                                      "UY" ),
	(    4308, "Uzbekistan",                                   "UZ" ),
	(    4309, "Holy See (Vatican City State)",                "VT" ),
	(    4310, "Saint Vincent and the Grenadines",             "VC" ),
	(    4311, "Venezuela",                                    "VE" ),
	(    4312, "Virgin Islands, British",                      "VI" ),
	(    4313, "Virgin Islands, U.S.",                         "VQ" ),
	(    4314, "Vietnam",                                      "VM" ),
	(    4315, "Vanuatu",                                      "NH" ),
	(    4316, "Wallis and Futuna",                            "WF" ),
	(    4317, "Samoa",                                        "WS" ),
	(    4318, "Yemen",                                        "YM" ),
	(    4319, "Mayotte",                                      "MF" ),
	(    4320, "South Africa",                                 "SF" ),
	(    4321, "Zambia",                                       "ZA" ),
	(    4322, "Zimbabwe",                                     "ZI" ),
	(    4323, "Metropolitan France",                          ""   ),
	(    4324, "Timor-Leste (East Timor)",                     "TT" ),
	(    4325, "Yugoslavia",                                   "YU" ) )
}

