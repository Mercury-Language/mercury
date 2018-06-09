%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: Ian MacLarty (maclarty@cs.mu.oz.au).
%
%---------------------------------------------------------------------------%
%
% A few language-related utility predicates.
%

:- module wix_language.

:- interface.

:- import_module wix.

:- pred det_translate(L::in, language::in, string::out) is det
    <= language_independent_tokens(L).

:- type lcid == int.

    % Convert a language to a MicroSoft locale code (lcid).
    %
:- pred language_to_lcid(language, lcid).
:- mode language_to_lcid(in, out) is det.
:- mode language_to_lcid(out, in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

:- import_module wix_util.

    % This table was obtained from:
    % http://www.microsoft.com/globaldev/reference/lcid-all.mspx
    % on 28 August 2005.
    %
language_to_lcid(afrikaans_south_africa, 1078).
language_to_lcid(albanian_albania, 1052).
language_to_lcid(amharic_ethiopia, 1118).
language_to_lcid(arabic_saudi_arabia, 1025).
language_to_lcid(arabic_algeria, 5121).
language_to_lcid(arabic_bahrain, 15361).
language_to_lcid(arabic_egypt, 3073).
language_to_lcid(arabic_iraq, 2049).
language_to_lcid(arabic_jordan, 11265).
language_to_lcid(arabic_kuwait, 13313).
language_to_lcid(arabic_lebanon, 12289).
language_to_lcid(arabic_libya, 4097).
language_to_lcid(arabic_morocco, 6145).
language_to_lcid(arabic_oman, 8193).
language_to_lcid(arabic_qatar, 16385).
language_to_lcid(arabic_syria, 10241).
language_to_lcid(arabic_tunisia, 7169).
language_to_lcid(arabic_u_a_e, 14337).
language_to_lcid(arabic_yemen, 9217).
language_to_lcid(armenian_armenia, 1067).
language_to_lcid(assamese, 1101).
language_to_lcid(azeri_cyrillic, 2092).
language_to_lcid(azeri_latin, 1068).
language_to_lcid(basque, 1069).
language_to_lcid(belarusian, 1059).
language_to_lcid(bengali, 1093).
language_to_lcid(bengali_bangladesh, 2117).
language_to_lcid(bosnian_bosnia_herzegovina, 5146).
language_to_lcid(bulgarian, 1026).
language_to_lcid(burmese, 1109).
language_to_lcid(catalan, 1027).
language_to_lcid(cherokee_united_states, 1116).
language_to_lcid(chinese_peoples_republic_of_china, 2052).
language_to_lcid(chinese_singapore, 4100).
language_to_lcid(chinese_taiwan, 1028).
language_to_lcid(chinese_hong_kong_sar, 3076).
language_to_lcid(chinese_macao_sar, 5124).
language_to_lcid(croatian, 1050).
language_to_lcid(croatian_bosnia_herzegovina, 4122).
language_to_lcid(czech, 1029).
language_to_lcid(danish, 1030).
language_to_lcid(divehi, 1125).
language_to_lcid(dutch_netherlands, 1043).
language_to_lcid(dutch_belgium, 2067).
language_to_lcid(edo, 1126).
language_to_lcid(english_united_states, 1033).
language_to_lcid(english_united_kingdom, 2057).
language_to_lcid(english_australia, 3081).
language_to_lcid(english_belize, 10249).
language_to_lcid(english_canada, 4105).
language_to_lcid(english_caribbean, 9225).
language_to_lcid(english_hong_kong_sar, 15369).
language_to_lcid(english_india, 16393).
language_to_lcid(english_indonesia, 14345).
language_to_lcid(english_ireland, 6153).
language_to_lcid(english_jamaica, 8201).
language_to_lcid(english_malaysia, 17417).
language_to_lcid(english_new_zealand, 5129).
language_to_lcid(english_philippines, 13321).
language_to_lcid(english_singapore, 18441).
language_to_lcid(english_south_africa, 7177).
language_to_lcid(english_trinidad, 11273).
language_to_lcid(english_zimbabwe, 12297).
language_to_lcid(estonian, 1061).
language_to_lcid(faroese, 1080).
language_to_lcid(farsi, 1065).
language_to_lcid(filipino, 1124).
language_to_lcid(finnish, 1035).
language_to_lcid(french_france, 1036).
language_to_lcid(french_belgium, 2060).
language_to_lcid(french_cameroon, 11276).
language_to_lcid(french_canada, 3084).
language_to_lcid(french_democratic_rep_of_congo, 9228).
language_to_lcid(french_cote_divoire, 12300).
language_to_lcid(french_haiti, 15372).
language_to_lcid(french_luxembourg, 5132).
language_to_lcid(french_mali, 13324).
language_to_lcid(french_monaco, 6156).
language_to_lcid(french_morocco, 14348).
language_to_lcid(french_north_africa, 58380).
language_to_lcid(french_reunion, 8204).
language_to_lcid(french_senegal, 10252).
language_to_lcid(french_switzerland, 4108).
language_to_lcid(french_west_indies, 7180).
language_to_lcid(frisian_netherlands, 1122).
language_to_lcid(fulfulde_nigeria, 1127).
language_to_lcid(fyro_macedonian, 1071).
language_to_lcid(gaelic_ireland, 2108).
language_to_lcid(gaelic_scotland, 1084).
language_to_lcid(galician, 1110).
language_to_lcid(georgian, 1079).
language_to_lcid(german_germany, 1031).
language_to_lcid(german_austria, 3079).
language_to_lcid(german_liechtenstein, 5127).
language_to_lcid(german_luxembourg, 4103).
language_to_lcid(german_switzerland, 2055).
language_to_lcid(greek, 1032).
language_to_lcid(guarani_paraguay, 1140).
language_to_lcid(gujarati, 1095).
language_to_lcid(hausa_nigeria, 1128).
language_to_lcid(hawaiian_united_states, 1141).
language_to_lcid(hebrew, 1037).
language_to_lcid(hindi, 1081).
language_to_lcid(hungarian, 1038).
language_to_lcid(ibibio_nigeria, 1129).
language_to_lcid(icelandic, 1039).
language_to_lcid(igbo_nigeria, 1136).
language_to_lcid(indonesian, 1057).
language_to_lcid(inuktitut, 1117).
language_to_lcid(italian_italy, 1040).
language_to_lcid(italian_switzerland, 2064).
language_to_lcid(japanese, 1041).
language_to_lcid(kannada, 1099).
language_to_lcid(kanuri_nigeria, 1137).
language_to_lcid(kashmiri, 2144).
language_to_lcid(kashmiri_arabic, 1120).
language_to_lcid(kazakh, 1087).
language_to_lcid(khmer, 1107).
language_to_lcid(konkani, 1111).
language_to_lcid(korean, 1042).
language_to_lcid(kyrgyz_cyrillic, 1088).
language_to_lcid(lao, 1108).
language_to_lcid(latin, 1142).
language_to_lcid(latvian, 1062).
language_to_lcid(lithuanian, 1063).
language_to_lcid(malay_malaysia, 1086).
language_to_lcid(malay_brunei_darussalam, 2110).
language_to_lcid(malayalam, 1100).
language_to_lcid(maltese, 1082).
language_to_lcid(manipuri, 1112).
language_to_lcid(maori_new_zealand, 1153).
language_to_lcid(marathi, 1102).
language_to_lcid(mongolian_cyrillic, 1104).
language_to_lcid(mongolian_mongolian, 2128).
language_to_lcid(nepali, 1121).
language_to_lcid(nepali_india, 2145).
language_to_lcid(norwegian_bokmal, 1044).
language_to_lcid(norwegian_nynorsk, 2068).
language_to_lcid(oriya, 1096).
language_to_lcid(oromo, 1138).
language_to_lcid(papiamentu, 1145).
language_to_lcid(pashto, 1123).
language_to_lcid(polish, 1045).
language_to_lcid(portuguese_brazil, 1046).
language_to_lcid(portuguese_portugal, 2070).
language_to_lcid(punjabi, 1094).
language_to_lcid(punjabi_pakistan, 2118).
language_to_lcid(quecha_bolivia, 1131).
language_to_lcid(quecha_ecuador, 2155).
language_to_lcid(quecha_peru, 3179).
language_to_lcid(rhaeto_romanic, 1047).
language_to_lcid(romanian, 1048).
language_to_lcid(romanian_moldava, 2072).
language_to_lcid(russian, 1049).
language_to_lcid(russian_moldava, 2073).
language_to_lcid(sami_lappish, 1083).
language_to_lcid(sanskrit, 1103).
language_to_lcid(sepedi, 1132).
language_to_lcid(serbian_cyrillic, 3098).
language_to_lcid(serbian_latin, 2074).
language_to_lcid(sindhi_india, 1113).
language_to_lcid(sindhi_pakistan, 2137).
language_to_lcid(sinhalese_sri_lanka, 1115).
language_to_lcid(slovak, 1051).
language_to_lcid(slovenian, 1060).
language_to_lcid(somali, 1143).
language_to_lcid(sorbian, 1070).
language_to_lcid(spanish_spain_modern_sort, 3082).
language_to_lcid(spanish_spain_traditional_sort, 1034).
language_to_lcid(spanish_argentina, 11274).
language_to_lcid(spanish_bolivia, 16394).
language_to_lcid(spanish_chile, 13322).
language_to_lcid(spanish_colombia, 9226).
language_to_lcid(spanish_costa_rica, 5130).
language_to_lcid(spanish_dominican_republic, 7178).
language_to_lcid(spanish_ecuador, 12298).
language_to_lcid(spanish_el_salvador, 17418).
language_to_lcid(spanish_guatemala, 4106).
language_to_lcid(spanish_honduras, 18442).
language_to_lcid(spanish_latin_america, 58378).
language_to_lcid(spanish_mexico, 2058).
language_to_lcid(spanish_nicaragua, 19466).
language_to_lcid(spanish_panama, 6154).
language_to_lcid(spanish_paraguay, 15370).
language_to_lcid(spanish_peru, 10250).
language_to_lcid(spanish_puerto_rico, 20490).
language_to_lcid(spanish_united_states, 21514).
language_to_lcid(spanish_uruguay, 14346).
language_to_lcid(spanish_venezuela, 8202).
language_to_lcid(sutu, 1072).
language_to_lcid(swahili, 1089).
language_to_lcid(swedish, 1053).
language_to_lcid(swedish_finland, 2077).
language_to_lcid(syriac, 1114).
language_to_lcid(tajik, 1064).
language_to_lcid(tamazight_arabic, 0414).
language_to_lcid(tamazight_latin, 1119).
language_to_lcid(tamil, 1097).
language_to_lcid(tatar, 1092).
language_to_lcid(telugu, 1098).
language_to_lcid(thai, 1054).
language_to_lcid(tibetan_bhutan, 2129).
language_to_lcid(tibetan_peoples_republic_of_china, 1105).
language_to_lcid(tigrigna_eritrea, 2163).
language_to_lcid(tigrigna_ethiopia, 1139).
language_to_lcid(tsonga, 1073).
language_to_lcid(tswana, 1074).
language_to_lcid(turkish, 1055).
language_to_lcid(turkmen, 1090).
language_to_lcid(uighur_china, 1152).
language_to_lcid(ukrainian, 1058).
language_to_lcid(urdu, 1056).
language_to_lcid(urdu_india, 2080).
language_to_lcid(uzbek_cyrillic, 2115).
language_to_lcid(uzbek_latin, 1091).
language_to_lcid(venda, 1075).
language_to_lcid(vietnamese, 1066).
language_to_lcid(welsh, 1106).
language_to_lcid(xhosa, 1076).
language_to_lcid(yi, 1144).
language_to_lcid(yiddish, 1085).
language_to_lcid(yoruba, 1130).
language_to_lcid(zulu, 1077).
language_to_lcid(hid_human_interface_device, 1279).

det_translate(Token, Language, Translation) :-
    ( if translate(Token, Language, WasTranslation) then
        Translation = WasTranslation
    else
        throw('new no_translation'(Token, Language))
    ).
