// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles

// eslint-disable-next-line no-global-assign
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  for (var i = 0; i < entry.length; i++) {
    newRequire(entry[i]);
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  return newRequire;
})({"characters.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.charactersVersion = exports.characters = void 0;
var simpChars = '梦缘丢并采乱亘亚亸来仑侣俣侠伥俩仓个们伦伟侧侦伪杰伧伞备佣传伛债伤倾偻仅佥侨仆侥偾雇价仪侬亿当侩俭傧俦侪尽偿优储俪罗攒傩傥俨凶兑儿兖内两册涂冻凛渎处凯凭别删刬刭则刚剥剐剀创铲划剧刘刽刿剑剂劲动务勚勋胜劳势绩劢励劝匀匦汇匮奁椟区协却厍厅厕厌厂厉厣参叆叇丛吴吕呙员呗吣唝唡问启哑衔啰啴唤丧乔单哟呛啬吗呜唢哔叹喽呕啧尝唛哗唠啸叽哓呒咴嘘咝哒哝哕嗳哙喷吨咛吓哜噜啮呖咙响喾严嘤啭嗫冁呓苏嘱国囵围园圆图团垧垯垱埯坝垭执坚垩垴埚阶尧报场塆块茔垲埘坞埙尘堑砖垫坠硗堕坛坟墙垦压垒圹垆坏垄坜壮壸壶寿够夹奂奥夺奋妆姗奸侄娱妩娄妇娅婳娲妫媪妈袅妪娴娆婵娇嫱嫒嬷嫔婴婶懒娈孙学孪宫寝实宁审写宽宠宝将专寻对导尴屃届尸屉屡层屦属冈峃峣岘岛峡崄崃岗岽峥嵛岚岁嵚嵝崭岖崂峤峄岙嵘岭屿岿峦巅巯卺帅师帐带帧帏帼帻帜币帮帱襕干几库庼厢厦荫厨厮庙庑废广廪庐弪张强弹弥弯彟彦雕径从徕复征彻恒悦怅闷凄恼恽恻爱惬悫怆恺忾态愠惨惭恸惯怄怂慭虑悭慑庆忧惫怜愦惮愤悯怃宪忆恳应怿懔恹怼懑惩怀悬忏惧欢恋戆戋戗戬戏战户抛挜挦挟扪扫抡挣挂拣扬换挥构揿损摇捣抢掴掼搂挚抠抟掺捞撑挠挢掸拨抚扑挞挝捡拥掳择击挡担携据挤举拟摈拧搁掷扩撷摆擞撸扰摅撵拢拦撄搀撺摄挛摊搅揽叙败敌数驱敛毙斓斩断旸昽时晋昼晕晖畅暂昵晔昙晓暧旷叠晒书会胧东栅杆栀枧条枭梼梾弃枨枣栋栈桊栖桠杨枫桢业极杩荣桤盘枪杠槚椠椁桨规桩乐枞楼标枢样朴树桦桡桥机椭横槔檩柽档桧检樯槟柠槛苘柜橹榈栉橼栎橱槠栌枥橥榇栊榉棂樱栏权椤栾榄钦欧欤归殁残殒殇殚殓殡歼杀壳毁殴医绒毵牦毡氇气氢氩氲凼决冱没冲沨沵况泄汹浉浐浕浃泾涢凉泪渌净凌沦渊涞浅涣减涡测浑凑浈愍涌汤溇沩准沟温湿沧灭涤荥滪沪滞渗卤浒滚满渔沤汉涟渍涨溆渐浆颍泼洁潜润浔溃滗涠涩浇涝涧渑泽泶浍淀浊浓泞济涛滥潍滨阔溅泺滤滢泻渖浏濒泸沥潇潆潴泷濑潋澜沣滠洒漓滩灏湾滦滟灾为乌烃无煅辉炼炜烟茕焕烦炀荧炝热颎炽烨焰灯炖烧烫焖营灿烛烩熏烬焘耀烁炉烂争爷尔床笺闸牍牵荦犊牺状狝狭狈狰犹狲犸呆狱狮奖独狯猃狞获猎犷兽獭献猕猡兹玙玚珏玱珰现琎珲珉玮琐瑶莹玛琏玑瑷环玺琼珑璎瓒瓯罂产亩毕画异畴痉痖瘆疯疡痪瘗疮疟瘘疗痨痫瘅疠瘪痴痒疖症疬癞癣瘿瘾痈瘫癫发皑皲皱蔼碍袄罢颁办绊绑镑谤盗盏监卢荡视眬众睁饱鲍辈贝钡绷笔闭边编贬变辩辫鳖宾饼睐眍瞒睑瞩钵铂驳补财蚕苍舱诧蝉馋谗缠阐颤矫硁硙硚硖砗砚碜长肠钞车陈衬称诚骋迟驰齿虫踌筹绸丑锄雏硕砀砜确码碛矶础触闯锤纯绰辞词赐聪葱蹿窜错达贷矿砺砾矾砻祎禄祸祯郸胆诞党祷邓递缔颠点电钓调谍御禅礼祢秃秾税秆棱钉顶锭订斗读赌镀锻缎队顿钝鹅额讹饿饵禀种谷稣积颖穑秽颓稳稆贰罚阀钒范贩饭访纺飞诽费纷粪丰锋风冯缝讽凤肤辐窝洼穷窑窭窥窍窦窃竖竞辅赋负讣缚该钙盖赶赣钢纲镐鸽阁铬给笋筜笕筝箓节龚巩贡钩购蛊顾关观馆贯龟闺轨诡贵辊锅过筑箧筱笃筛筚箦篓蓑箪简篑箫檐签帘骇韩号阂鹤贺轰鸿红后篮藤箨籁笼钥笾簖篱箩吁粤护华话还缓黄谎贿讳诲绘荤伙货糁模粮粝籴粜纠纪纣约纡纥纨纫纹纳纽纾纰纼纱纮纸级纭纴细绂绁绅纻饥迹讥鸡缉辑蓟计记际继荚颊贾钾驾间艰缄茧荐鉴践贱见键绍绀绋绐绌终组绗结绝绦绔绞络绚绖统丝绛绢绡绠绨绣绤绥经舰饯蒋讲酱胶骄铰脚饺缴轿较诫紧锦谨进综缍绿绻绶维绹绾网缀纶绺绮绽绫绵绲缁绯缗绪绬绱缃缂缌缅纬缑缈练缏缇荆茎鲸惊颈静镜旧驹锯鹃觉诀钧军缊总萦缙缢缒绉缣缞缜缟缛县缡缩纵缧纤缦絷缕缥缫缪襁缯织缮缭骏开颗课裤夸亏绕缋绳缰缳缲绎缤缱颣缬纩续缨缵缆馈蜡腊莱赖蓝阑兰谰览镭类离鲤丽骂罴羁芈羟羡义习翙翚隶联莲连镰脸链辆谅辽镣临邻鳞赁龄铃灵领馏龙聋翘翱耧耢圣闻声耸聩聂职聍听陇芦颅虏鲁赂录陆驴铝轮论萝逻锣骡骆蚂马买麦卖迈脉馒蛮肃胁胫脱胀谩猫锚铆贸么镁门锰谜觅闽鸣铭谬肾胨腘脶脑肿肷腽腻脍脓脐膑癯谋钠难闹馁酿鸟镊镍胪臜脏脔卧皋与兴铺舣钮农诺鸥庞赔鹏舻艳艹刍苎骗飘频贫苹评颇谱齐骑岂讫荙庄苋钎铅迁谦钱钳谴蔷锹亲轻顷请趋躯苌萚万莴叶荭荮苇药龋颧鹊让饶韧认软锐闰萨鳃赛莼莳荪莜苁荜骚闪陕赡赏赊设蒌茑荨蒇荞荬芸莸荛蒉芜萧蓣诗蚀识驶适释饰试输赎术荟芗姜莶荠荩艺薮苈双谁顺说饲颂讼诵诉虽随锁蔺蕲蕴藓蔹茏蓠虚贪谭谈讨腾誊锑题体贴铁蛱蜕蚬铜头鸵驮驼袜顽猬虾虱蜗螀蛳蚁萤韦违谓卫钨诬雾误锡蝼蛰蝈螨蟏虮蛲蛏蝇虿蝎蛴蝾蛎袭铣辖锨鲜咸贤闲显险馅镶乡详响项销蔑袆衮谐谢锌衅锈须许轩袯裈装裢选询驯训讯逊鸦鸭讶阉盐颜阎谚验鸯阳养褛亵幞裥杂裣裆褴觃觇觋遥谣页铱颐遗诣议谊译阴银饮隐觍觎觊觏觑觐觌觞觯讠讧讦讱讪鹰赢踊咏邮铀诱舆鱼语誉讷谌讻诃诊注证诂诋讵诈诒诏诐诇诎诅詟诩诟诠诘诜诙诖预驭鸳辕远愿跃阅云郧陨运酝韵载赞赃凿责贼赠轧诔诛诓诳诶诮诰谇谄谆诿诤诹诼谂谀谞谝谥诨铡斋辗账赵辙锗这贞针镇阵谔谛谏谕谙诸谖谒诌谧谑谡谟谪讴郑质钟轴骤猪着贮铸谲谮谯谵诪谫雠谶谠谳驻转赚锥赘资踪邹钻豮狸贠鼗靥赝赜贳赀贶贻贲赅赈赇赒赉赓赕赍赆赗赙贽赟黉凫赑赪趱陉陧邝邬邺跶蜷跄跸跖蹒郏郐郄郓郦跷趸跻踯跞踬蹰跹蹑躜躏轪轫轭轷轸轱轵轺轲轶轼辂辁辀轾辄辎辋辍辇辌辏辒毂莺蓦蓥辘辚辔轹轳迩逦酦酾酽钆钇钌钊钋钐狍钏钗钍钕钯钫钘钭钚钤钣钑钬钛钪铌铈钶钴钹铍钰钸钿饧饨饩饪饫饬饴饷饽余馄馇馊馍馐馑馓馔馕铊铉铋钷钺钲钼钽锎铏铒铪铳铚铨铢铫铦铑铷铟闩闫闱闳闵闶闼闾阃阄阆阈阊阋阌阍阏阒阕阖阗阙阚丬铵铥铕铯铐铞锉镅锒铤铗铻镯锊锓铘锃锔锇铓铖锆锂铽锍锞锖锫锩铔锕锟锱铮锛锬锜锠锢铼镎锝锪钔锴锳锷钖锽锸锲锘骞锾锶锿镕镉镈镃镏铠铩锼镒镋镓镌镞镟镆镙驵驷驸驺驿驽骀骁骅骈骊骐骒骓骖骘骛骜骝骟骠骢骣骥骧纟镠镝铿锵镗镘镛镖镂錾镚铧镤镪铙铴铹镦镡镫镢镨锏镄顼镮铎铛镱镬镔镲锧镴铄镳镥镧镵镩銮韪韫韬闬阇阘闿阓阛腼霁雳霭靓飑飒飓飕飙齑于鞒鞑鞯韨鞴顸颀龛颃颌颉颏颒颋颕颔颚颙颛颡颟颢颥颦颞飐飔飖飗饤饦饳饸饹饻馂饾肴馃馉馎馌飨餍鸠鸢鸨鸩鸪鸫鸬鸲鸱鸶鸸鸷鸹鸺鸾鹁鹂鹄鹆鹇鹈鹉鹋鹌鹎鹑鹕鹗鹚鹛鹜鹞鹣鹦鹧鹨鹩鹪鹫鹬鹱鹭鹳驲骃骎骍骔骙骕骦骉鲠髅髌髋鬓郁魉魇鱽鱾鲀鲂鱿鲄麸鲅鲆鲌鲉鲧鲏鲇鲐鲋鲊鲒鲘鲕鲖鲔鲛鲑鲓鲪鳀鲝鲩鲨鲬鲻鲯鲭鲞鲷酰鹾鲴鲱鲵鲲鲳鲮鲰鲶鲺鲹鲫鳊鳈鲗鳂鲽鳇鳅鲾鳄鳆鳁鳒鳑鳋鲥鳏鳎鳐鳍鲢鳌鳓鳘鲦鲣鳗龀龃龅龆龇龈龉龊龌黾鼋鼍鲈鲎鲚鲟鲡鲼鳛鳔鳉鳙鳕鳟鳝鳜鲙鳣鳡鳢鲿鳠鹘黩黪鼹齄鸤鸰鸮鸴鸻鸼鹀鹐鹓鹍鹒鹙鹖鸧鹟鹠鹡鹢鹝鹥鹔鹯鹲鹴黡鼌龁龂飚村钜㖞㧑㧟㱮䁖䇲䌶䌷䌸䌹䌺䍁䞍䴓䴔䴕䴖䴗䴘䴙伣俫刹厐叁呐垅姹弑悮戯捝揾梿棁榅煴疭瞆祃窎筼肮蔂蕰訚迳酂钅锺霡飏饣鲃鳚余硷耻囱恶珐厩扦瓮嚣扎伫谘藁腌腭沓蚝籼糇隽线莅讬䜣铇眦碜碱';
var tradChars = '夢緣丟並採亂亙亞嚲來侖侶俁俠倀倆倉個們倫偉側偵偽傑傖傘備傭傳傴債傷傾僂僅僉僑僕僥僨僱價儀儂億當儈儉儐儔儕盡償優儲儷羅攢儺儻儼兇兌兒兗內兩冊塗凍凜瀆處凱憑別刪剗剄則剛剝剮剴創鏟劃劇劉劊劌劍劑勁動務勩勛勝勞勢績勱勵勸勻匭匯匱奩櫝區協卻厙廳廁厭廠厲厴參靉靆叢吳呂咼員唄唚嗊啢問啓啞銜囉嘽喚喪喬單喲嗆嗇嗎嗚嗩嗶嘆嘍嘔嘖嘗嘜嘩嘮嘯嘰嘵嘸噅噓噝噠噥噦噯噲噴噸嚀嚇嚌嚕嚙嚦嚨嚮嚳嚴嚶囀囁囅囈蘇囑國圇圍園圓圖團坰墶壋垵壩埡執堅堊堖堝階堯報場壪塊塋塏塒塢塤塵塹磚墊墜磽墮壇墳牆墾壓壘壙壚壞壟壢壯壼壺壽夠夾奐奧奪奮妝姍姦姪娛嫵婁婦婭嫿媧媯媼媽裊嫗嫻嬈嬋嬌嬙嬡嬤嬪嬰嬸懶孌孫學孿宮寢實寧審寫寬寵寶將專尋對導尷屓屆屍屜屢層屨屬岡嶨嶢峴島峽嶮崍崗崬崢崳嵐歲嶔嶁嶄嶇嶗嶠嶧嶴嶸嶺嶼巋巒巔巰巹帥師帳帶幀幃幗幘幟幣幫幬襴幹幾庫廎廂廈蔭廚廝廟廡廢廣廩廬弳張強彈彌彎彠彥彫徑從徠複徵徹恆悅悵悶悽惱惲惻愛愜愨愴愷愾態慍慘慚慟慣慪慫憖慮慳懾慶憂憊憐憒憚憤憫憮憲憶懇應懌懍懨懟懣懲懷懸懺懼歡戀戇戔戧戩戲戰戶拋掗撏挾捫掃掄掙掛揀揚換揮構撳損搖搗搶摑摜摟摯摳摶摻撈撐撓撟撣撥撫撲撻撾撿擁擄擇擊擋擔攜據擠舉擬擯擰擱擲擴擷擺擻擼擾攄攆攏攔攖攙攛攝攣攤攪攬敘敗敵數驅斂斃斕斬斷暘曨時晉晝暈暉暢暫暱曄曇曉曖曠曡曬書會朧東柵桿梔梘條梟檮棶棄棖棗棟棧棬棲椏楊楓楨業極榪榮榿盤槍槓檟槧槨槳規樁樂樅樓標樞樣樸樹樺橈橋機橢橫橰檁檉檔檜檢檣檳檸檻檾櫃櫓櫚櫛櫞櫟櫥櫧櫨櫪櫫櫬櫳櫸欞櫻欄權欏欒欖欽歐歟歸歿殘殞殤殫殮殯殲殺殼毀毆醫絨毿犛氈氌氣氫氬氳氹決沍沒沖渢濔況洩洶溮滻濜浹涇溳涼淚淥淨淩淪淵淶淺渙減渦測渾湊湞湣湧湯漊溈準溝溫濕滄滅滌滎澦滬滯滲鹵滸滾滿漁漚漢漣漬漲漵漸漿潁潑潔潛潤潯潰潷潿澀澆澇澗澠澤澩澮澱濁濃濘濟濤濫濰濱闊濺濼濾瀅瀉瀋瀏瀕瀘瀝瀟瀠瀦瀧瀨瀲瀾灃灄灑灕灘灝灣灤灧災為烏烴無煆輝煉煒煙煢煥煩煬熒熗熱熲熾燁燄燈燉燒燙燜營燦燭燴燻燼燾燿爍爐爛爭爺爾牀箋閘牘牽犖犢犧狀獮狹狽猙猶猻獁獃獄獅獎獨獪獫獰獲獵獷獸獺獻獼玀茲璵瑒玨瑲璫現璡琿瑉瑋瑣瑤瑩瑪璉璣璦環璽瓊瓏瓔瓚甌罌產畝畢畫異疇痙瘂瘮瘋瘍瘓瘞瘡瘧瘻療癆癇癉癘癟癡癢癤癥癧癩癬癭癮癰癱癲發皚皸皺藹礙襖罷頒辦絆綁鎊謗盜盞監盧蕩視矓眾睜飽鮑輩貝鋇繃筆閉邊編貶變辯辮鱉賓餅睞瞘瞞瞼矚缽鉑駁補財蠶蒼艙詫蟬饞讒纏闡顫矯硜磑礄硤硨硯磣長腸鈔車陳襯稱誠騁遲馳齒蟲躊籌綢醜鋤雛碩碭碸確碼磧磯礎觸闖錘純綽辭詞賜聰蔥躥竄錯達貸礦礪礫礬礱禕祿禍禎鄲膽誕黨禱鄧遞締顛點電釣調諜禦禪禮禰禿穠稅稈稜釘頂錠訂鬥讀賭鍍鍛緞隊頓鈍鵝額訛餓餌稟種穀穌積穎穡穢頹穩穭貳罰閥釩範販飯訪紡飛誹費紛糞豐鋒風馮縫諷鳳膚輻窩窪窮窯窶窺竅竇竊豎競輔賦負訃縛該鈣蓋趕贛鋼綱鎬鴿閣鉻給筍簹筧箏籙節龔鞏貢鉤購蠱顧關觀館貫龜閨軌詭貴輥鍋過築篋篠篤篩篳簀簍簑簞簡簣簫簷簽簾駭韓號閡鶴賀轟鴻紅後籃籐籜籟籠鑰籩籪籬籮籲粵護華話還緩黃謊賄諱誨繪葷夥貨糝糢糧糲糴糶糾紀紂約紆紇紈紉紋納紐紓紕紖紗紘紙級紜紝細紱紲紳紵饑跡譏雞緝輯薊計記際繼莢頰賈鉀駕間艱緘繭薦鑒踐賤見鍵紹紺紼紿絀終組絎結絕縧絝絞絡絢絰統絲絳絹綃綆綈繡綌綏經艦餞蔣講醬膠驕鉸腳餃繳轎較誡緊錦謹進綜綞綠綣綬維綯綰網綴綸綹綺綻綾綿緄緇緋緡緒緓緔緗緙緦緬緯緱緲練緶緹荊莖鯨驚頸靜鏡舊駒鋸鵑覺訣鈞軍縕總縈縉縊縋縐縑縗縝縞縟縣縭縮縱縲纖縵縶縷縹繅繆繈繒織繕繚駿開顆課褲誇虧繞繢繩韁繯繰繹繽繾纇纈纊續纓纘纜饋蠟臘萊賴藍闌蘭讕覽鐳類離鯉麗罵羆羈羋羥羨義習翽翬隸聯蓮連鐮臉鏈輛諒遼鐐臨鄰鱗賃齡鈴靈領餾龍聾翹翺耬耮聖聞聲聳聵聶職聹聽隴蘆顱虜魯賂錄陸驢鋁輪論蘿邏鑼騾駱螞馬買麥賣邁脈饅蠻肅脅脛脫脹謾貓錨鉚貿麼鎂門錳謎覓閩鳴銘謬腎腖膕腡腦腫膁膃膩膾膿臍臏臒謀鈉難鬧餒釀鳥鑷鎳臚臢髒臠臥臯與興鋪艤鈕農諾鷗龐賠鵬艫艷艸芻苧騙飄頻貧蘋評頗譜齊騎豈訖薘莊莧釺鉛遷謙錢鉗譴薔鍬親輕頃請趨軀萇蘀萬萵葉葒葤葦藥齲顴鵲讓饒韌認軟銳閏薩鰓賽蒓蒔蓀蓧蓯蓽騷閃陝贍賞賒設蔞蔦蕁蕆蕎蕒蕓蕕蕘蕢蕪蕭蕷詩蝕識駛適釋飾試輸贖術薈薌薑薟薺藎藝藪藶雙誰順說飼頌訟誦訴雖隨鎖藺蘄蘊蘚蘞蘢蘺虛貪譚談討騰謄銻題體貼鐵蛺蛻蜆銅頭鴕馱駝襪頑蝟蝦蝨蝸螿螄蟻螢韋違謂衛鎢誣霧誤錫螻蟄蟈蟎蠨蟣蟯蟶蠅蠆蠍蠐蠑蠣襲銑轄鍁鮮鹹賢閑顯險餡鑲鄉詳響項銷衊褘袞諧謝鋅釁鏽須許軒襏褌裝褳選詢馴訓訊遜鴉鴨訝閹鹽顏閻諺驗鴦陽養褸褻襆襇雜襝襠襤覎覘覡遙謠頁銥頤遺詣議誼譯陰銀飲隱覥覦覬覯覷覲覿觴觶訁訌訐訒訕鷹贏踴詠郵鈾誘輿魚語譽訥諶訩訶診註證詁詆詎詐詒詔詖詗詘詛讋詡詬詮詰詵詼詿預馭鴛轅遠願躍閱雲鄖隕運醞韻載贊贓鑿責賊贈軋誄誅誆誑誒誚誥誶諂諄諉諍諏諑諗諛諝諞諡諢鍘齋輾賬趙轍鍺這貞針鎮陣諤諦諫諭諳諸諼謁謅謐謔謖謨謫謳鄭質鐘軸驟豬著貯鑄譎譖譙譫譸譾讎讖讜讞駐轉賺錐贅資蹤鄒鑽豶貍貟鞀靨贗賾貰貲貺貽賁賅賑賕賙賚賡賧賫贐賵賻贄贇黌鳧贔赬趲陘隉鄺鄔鄴躂踡蹌蹕蹠蹣郟鄶郤鄆酈蹺躉躋躑躒躓躕躚躡躦躪軑軔軛軤軫軲軹軺軻軼軾輅輇輈輊輒輜輞輟輦輬輳轀轂鶯驀鎣轆轔轡轢轤邇邐醱釃釅釓釔釕釗釙釤麅釧釵釷釹鈀鈁鈃鈄鈈鈐鈑鈒鈥鈦鈧鈮鈰鈳鈷鈸鈹鈺鈽鈿餳飩餼飪飫飭飴餉餑餘餛餷餿饃饈饉饊饌饢鉈鉉鉍鉕鉞鉦鉬鉭鐦鉶鉺鉿銃銍銓銖銚銛銠銣銦閂閆闈閎閔閌闥閭閫鬮閬閾閶鬩閿閽閼闃闋闔闐闕闞爿銨銩銪銫銬銱銼鎇鋃鋌鋏鋙鐲鋝鋟鋣鋥鋦鋨鋩鋮鋯鋰鋱鋶錁錆錇錈錏錒錕錙錚錛錟錡錩錮錸鎿鍀鍃鍆鍇鍈鍔鍚鍠鍤鍥鍩騫鍰鍶鎄鎔鎘鎛鎡鎦鎧鎩鎪鎰鎲鎵鐫鏃鏇鏌鏍駔駟駙騶驛駑駘驍驊駢驪騏騍騅驂騭騖驁騮騸驃驄驏驥驤糸鏐鏑鏗鏘鏜鏝鏞鏢鏤鏨鏰鏵鏷鏹鐃鐋鐒鐓鐔鐙钁鐠鐧鐨頊鐶鐸鐺鐿鑊鑌鑔鑕鑞鑠鑣鑥鑭鑱鑹鑾韙韞韜閈闍闒闓闠闤靦霽靂靄靚颮颯颶颼飆齏於鞽韃韉韍韝頇頎龕頏頜頡頦頮頲頴頷顎顒顓顙顢顥顬顰顳颭颸颻飀飣飥飿餄餎餏餕餖餚餜餶餺饁饗饜鳩鳶鴇鴆鴣鶇鸕鴝鴟鷥鴯鷙鴰鵂鸞鵓鸝鵠鵒鷳鵜鵡鶓鵪鵯鶉鶘鶚鶿鶥鶩鷂鶼鸚鷓鷚鷯鷦鷲鷸鸌鷺鸛馹駰駸騂騌騤驌驦驫鯁髏髕髖鬢鬱魎魘魛魢魨魴魷魺麩鮁鮃鮊鮋鯀鮍鮎鮐鮒鮓鮚鮜鮞鮦鮪鮫鮭鮳鮶鯷鮺鯇鯊鯒鯔鯕鯖鯗鯛醯鹺鯝鯡鯢鯤鯧鯪鯫鯰鯴鰺鯽鯿鰁鰂鰃鰈鰉鰍鰏鱷鰒鰮鰜鰟鰠鰣鰥鰨鰩鰭鰱鰲鰳鰵鰷鰹鰻齔齟齙齠齜齦齬齪齷黽黿鼉鱸鱟鱭鱘鱺鱝鰼鰾鱂鱅鱈鱒鱔鱖鱠鱣鱤鱧鱨鱯鶻黷黲鼴齇鳲鴒鴞鴬鴴鵃鵐鵮鵷鵾鶊鶖鶡鶬鶲鶹鶺鷁鷊鷖鷫鸇鸏鸘黶鼂齕齗飈邨鉅喎撝擓殨瞜筴䊷紬縳絅䋙繸䝼鳾鵁鴷鶄鶪鷈鷿俔倈剎龎叄吶壠奼弒悞戱挩搵槤梲榲熅瘲瞶禡窵篔骯虆薀誾逕酇釒鍾霢颺飠䰾䲁餘礆恥囪惡琺廄扡甕囂紮佇諮槁醃齶遝蠔秈餱雋線蒞託訢鉋眥磣鹼';
var charactersNoDef = [];

for (var i = 0; i < simpChars.length; i++) {
  charactersNoDef.push(simpChars.charAt(i) + '|' + tradChars.charAt(i));
}

var charactersWithDef = "\u672C|\u672C\n\u8336|\u8336|Tea\n\u4E66|\u66F8\n\u66F8|\u66F8\n\u500B|\u500B\n\u5757|\u584A\n\u584A|\u584A\n\u5F35|\u5F35\n\u83EF|\u83EF\n\u5BB6\u4EBA|\u5BB6\u4EBA|Family|noun\n\u4E0D\u5BA2\u6C14|\u4E0D\u5BA2\u6C23|You are welcome|expression\n\u4E0D\u5BA2\u6C23|\u4E0D\u5BA2\u6C23|You are welcome|expression\n\u8B1D\u8B1D|\u8B1D\u8B1D|Thank you|expression\n\u8C22\u8C22|\u8B1D\u8B1D|Thank you|expression\n\u8C22|\u8B1D|Thank|verb|Can be doubled to say \u8C22\u8C22 (Thank you)\n\u8B1D|\u8B1D|Thank|verb|Can be doubled to say \u8B1D\u8B1D (Thank you)\n\u4F26\u6566|\u502B\u6566|London|City\n\u502B\u6566|\u502B\u6566|London|City\n\u53F0\u7063|\u53F0\u7063|Taiwan|Country\n\u53F0\u6E7E|\u53F0\u7063|Taiwan|Country\n\u54EA\u513F|\u54EA\u5152|Where|interrogative adverb\n\u7EBD\u7EA6|\u7D10\u7D04|New York|City\n\u9999\u6E2F|\u9999\u6E2F|Hong Kong|City\n\u4F60\u597D|\u4F60\u597D|Hello|greeting|literally: 'you good'?\n\u9AD8|\u9AD8|High/Tall|adjective\n\u5174|\u8208|Happy|adjective\n\u5F88|\u5F88|Very|adverb\n\u8BC6|\u8B58|Knowledge|noun\n\u8BA4|\u8A8D|Recognise|verb\n\u4E5F|\u4E5F|Also|adverb\n\u6211|\u6211|I|pronoun\n\u4ED6|\u4ED6|He|pronoun\n\u597D|\u597D|Good|adjective\n\u5B66|\u5B78|Understand/Learn|noun/verb|Forms a compound word \u5B66\u751F(student)\n\u4F60|\u4F60|You|pronoun|literally: 'you good'?\n\u8208|\u8208|Happy|adjective\n\u8B58|\u8B58|Knowledge|noun\n\u8A8D|\u8A8D|Recognise|verb\n\u674E|\u674E|Li|surname\n\u59D3|\u59D3|surname|noun\n\u4E5D|\u4E5D|Nine|number\n\u96F6|\u96F6|Zero|number\n\u767E|\u767E|100|number\n\u4E09|\u4E09|Three|number\n\u5B57|\u5B57|Word|noun\n\u518D|\u518D|Again|adverb|Forms a compound word \u518D\u898B (Goodbye, literally again see).\n\u4E8C|\u4E8C|Two|number\n\u4E00|\u4E00|One|number\n\u4E94|\u4E94|Five|number\n\u5341|\u5341|Ten|number|Can be combined with other numbers to create multiples of ten \u4E03\u5341 (Seventy)\n\u516D|\u516D|Six|number\n\u53EB|\u53EB|Call|verb\n\u9C7C|\u9B5A|Fish|noun\n\u559D|\u559D|Drink|verb\n\u996D|\u98EF|Rice|noun|\n\u9762|\u9762|Noodles|noun\n\u5403|\u5403|Eat|verb\n\u6C34|\u6C34|Water|noun\n\u8336|\u8336|Tea|noun\n\u9B5A|\u9B5A|Fish|noun\n\u4E0D|\u4E0D|No/Not|Determiner/Adverb\n\u98EF|\u98EF|Rice|noun|\n\u4E48|\u9EBC|Interrogative sentence|suffix|\u4EC0\u4E48(What), \u600E\u4E48 (How), \u8FD9\u4E48 (Like this), \u591A\u4E48 (how; what; so; such)\n\u4E03|\u4E03|Seven|number\n\u5462|\u5462|Interrogative particle|suffix\n\u9EBC|\u9EBC|Interrogative particle|suffix|\u4EC0\u4E48(What), \u600E\u4E48 (How), \u8FD9\u4E48 (Like this), \u591A\u4E48 (how; what; so; such)\n\u540D|\u540D|Name|noun\n\u738B|\u738B|Wang|name\n\u89C1|\u898B|See|verb|Forms a compound word \u518D\u898B (Goodbye, literally again see).\n\u898B|\u898B|See|Verb|Forms a compound word \u518D\u898B (Goodbye, literally again see).\n\u5979|\u5979|She/Her|pronoun\n\u4EEC|\u5011|Plural|suffix|Turns \u5979 (she) into them/they plural (females) \u5979\u4EEC or \u4ED6 (he) into them/they \u4ED6\u5011 (males)\n\u662F|\u662F|Be|verb\n\u5417|\u55CE|Interrogative particle|suffix|At the end of a sentence it creates a yes/no question.\n\u751F|\u751F|Pupil|noun|forms a compound word \u5B78\u751F(student)|This word has a lot of meanings; live, grow, pupil, disciple.\n\u5E08|\u5E2B|Expert|noun|orms a compound word \u8001\u5E2B (teacher)\n\u5011|\u5011|Plural|suffix|Turns \u5979 (she) into them/they plural (females) \u5979\u4EEC or \u4ED6 (he) into them/they \u4ED6\u5011 (males)\n\u8001|\u8001|Experienced|noun/verb|forms a compound word \u8001\u5E2B (teacher)\n\u533B|\u91AB|Doctor/medicine|noun|forms a compound word \u91AB\u751F  (doctor)|This word has a lot of meanings; live, grow, pupil, disciple.\n\u55CE|\u55CE|Interrogative particle|suffix|At the end of a sentence it creates a yes/no question.\n\u5E2B|\u5E2B|Expert|noun|forms a compound word \u8001\u5E2B (teacher)\n\u91AB|\u91AB|Doctor/medicine|noun|forms a compound word \u91AB\u91AB\u751F  (doctor)|This word has a lot of meanings; live, grow, pupil, disciple.\n\u8D77\u53F8|\u8D77\u53F8\n\u5ABD|\u5ABD|Mum|particle noun|doubles to make \u5ABD\u5ABD(Mum)\n\u5988|\u5ABD|Mum|particle noun|doubles to make \u5988(Mum)\n\u810F|\u81DF\nundefined|undefined\n\u8F7B|\u8F15|Light/Small|adjective\n\u9EB5|\u9EB5|Noodles|noun\n\u8282|\u7BC0\n\u56FD|\u570B|Country|noun\n\u6CD5|\u6CD5\n\u529B|\u529B\n\u540E|\u5F8C\n\u5229|\u5229\n\u60F3|\u60F3\n\u570B|\u570B|Country|noun\n\u5F8C|\u5F8C\n\u52AA|\u52AA\n\u5B63|\u5B63\n\u8FD8|\u9084\n\u6700|\u6700\n\u5927|\u5927|Large|Adjective|Can also be used in a compound to form the word \u52A0\u62FF\u5927 (Canada)\n\u767D|\u767D\n\u9152|\u9152\n\u671B|\u671B\n\u610F|\u610F\n\u5931|\u5931\n\u7BC0|\u7BC0\n\u9084|\u9084\n\u70B9|\u9EDE\n\u9EDE|\u9EDE\n\u513F|\u5152|Child|noun\n\u5152|\u5152|Child|noun\n\u7684|\u7684|his/her/my/your|posessive suffix|Used after a word it indicates the belonging of; for example combining \u6211 (I) making a \u6211\u7684(my) or \u4ED6 (he) making \u4ED6\u7684(his)\n\u516B|\u516B|Eight|number\n\u6B21|\u6B21\n\u5143|\u5143|Yen|Noun|Currency of Mainland China \uD83C\uDDE8\uD83C\uDDF3\n\u56DB|\u56DB|Four|number\n\u5F20|\u5F35\n\u660E|\u660E\n\u60A8|\u60A8\n\u534E|\u83EF\n\u4EC0|\u751A|What|interrogative adverb\n\u5176|\u5176\n\u4EBA|\u4EBA|Person|noun|You will almost always see this in a compound word like \u4E2D\u56FD\u4EBA (Chinese person; literally China person).\n\u5929|\u5929|Day|noun|Can be combined with \u4ECA (now) to make the compound word \u4ECA\u5929(today).\n\u5730|\u5730\n\u706B|\u706B\n\u7406|\u7406\n\u8BFE|\u8AB2\n\u9505|\u934B\n\u5C0F|\u5C0F|Small|adjective\n\u4E2A|\u500B\n\u6709|\u6709\n\u934B|\u934B\n\u8AB2|\u8AB2\n\u9986|\u9928\n\u5561|\u5561\n\u5496|\u5496\n\u56FE|\u5716\n\u4E0B|\u4E0B\n\u9928|\u9928\n\u5716|\u5716\n\u6234|\u6234\n\u751C|\u751C\n\u7F8E|\u7F8E|Beautiful/America|Verb/Noun|This can form a compound word with \u56FD (country) to make \u7F8E\u56FD (America) or \u7F8E\u56FD\u4EBA (American person; literally America man)\n\u672F|\u8853\n\u7237|\u723A\n\u8853|\u8853\n\u723A|\u723A\n\u8FC7|\u904E\n\u904E|\u904E\n\u6CB9|\u6CB9\n\u6761|\u689D\n\u689D|\u689D\n\u4EAC|\u4EAC|Commonly used with \u5317 (North) to make \u5317\u4EAC(Beijing. Literally: North capital)\n\u79CB|\u79CB\n\u5E74|\u5E74\n\u53BB|\u53BB\n\u6765|\u4F86\n\u5317|\u5317|North|noun|Commonly used with \u4EAC (Capital) to make \u5317\u4EAC(Beijing. Literally: North capital)\n\u4E86|\u4E86|Adverb|Indicates something is complete or finished\n\u4F86|\u4F86\n\u4FE1|\u4FE1\n\u5E94|\u61C9\n\u5FC3|\u5FC3|Heart|noun\n\u8BE5|\u8A72\n\u8A72|\u8A72\n\u61C9|\u61C9\n\u8D5B|\u8CFD\n\u5728|\u5728|In|Preposition\n\u6BCF|\u6BCF\n\u8981|\u8981\n\u6BD4|\u6BD4\n\u6708|\u6708\n\u73B0|\u73FE\n\u7EC3|\u7DF4\n\u4E60|\u7FD2\n\u8CFD|\u8CFD\n\u73FE|\u73FE\n\u7DF4|\u7DF4\n\u7FD2|\u7FD2\n\u9519|\u932F\n\u8003|\u8003\n\u8BD5|\u8A66\n\u5F97|\u5F97\n\u8A66|\u8A66\n\u932F|\u932F\n\u6CF0|\u6CF0\n\u5F61|\u5F61\n\u96E8|\u96E8\n\u52C7|\u52C7\n\u5B89|\u5B89\n\u7F85|\u7F85\n\u5510|\u5510\n\u667A|\u667A\n\u9A8F|\u99FF\n\u5289|\u5289\n\u658C|\u658C\n\u54F2|\u54F2\n\u671D|\u671D\n\u626C|\u63DA\n\u745E|\u745E\n\u8DEF|\u8DEF\n\u6B23|\u6B23\n\u99FF|\u99FF\n\u63DA|\u63DA\n\u6602|\u6602\n\u7D0D|\u7D0D\n\u591A|\u591A|Many|Compound: Determiner, pronoun and adjective|Can form compound question endings such as \u591A\u5C11 (what is?)\n\u840A|\u840A\n\u4E14|\u4E14\n\u800C|\u800C\n\u59B3|\u59B3|You(Female)|pronoun\n\u7C73|\u7C73\n\u671F|\u671F|Meet|verb\n\u661F|\u661F\n\u65E5|\u65E5|Sun/Day|noun\n\u5468|\u9031\n\u9031|\u9031\n\u82F1|\u82F1|Excellent/British|noun/adjective|This can form a compound word with \u56FD (country) to make \u82F1\u56FD (Britain) or \u82F1\u56FD\u4EBA (British person; literally Britain country man)\n\u8A9E|\u8A9E\n\u52D5|\u52D5\n\u8A5E|\u8A5E\n\u7F6E|\u7F6E\n\u7269|\u7269\n\u9593|\u9593\n\u4ED8|\u4ED8\n\u5BB6|\u5BB6|Family|noun\n\u65CF|\u65CF\n\u524D|\u524D\n\u526F|\u526F\n\u6642|\u6642\n\u696D|\u696D\n\u5BB9|\u5BB9\n\u8077|\u8077\n\u5F62|\u5F62\n\u6CBB|\u6CBB\n\u5834|\u5834\n\u56DE|\u56DE\n\u6240|\u6240\n\u8EAB|\u8EAB\n\u6570|\u6578\n\u653F|\u653F\n\u5C5E|\u5C6C\n\u6027|\u6027\n\u6578|\u6578\n\u5C6C|\u5C6C\n\u677F|\u677F\n\u4E2D|\u4E2D|Middle|adjective|You will mostly see this character as a compound with \u56FD (country) making \u4E2D\u56FD (China)\n\u7DE8|\u7DE8\n\u96C6|\u96C6\n\u63B2|\u63B2\n\u793A|\u793A\n\u554F|\u554F\n\u8A00|\u8A00\n\u984C|\u984C\n\u4F5C|\u4F5C\n\u63A2|\u63A2\n\u6210|\u6210\n\u6C7A|\u6C7A\n\u89A7|\u89A7\n\u89E3|\u89E3\n\u8CA2|\u8CA2\n\u5BFE|\u5BFE\n\u5FDC|\u5FDC\n\u732E|\u737B\n\u6295|\u6295\n\u65B0|\u65B0\n\u6C17|\u6C17\n\u7A3F|\u7A3F\n\u898F|\u898F\n\u7740|\u8457\n\u7AE0|\u7AE0\n\u6587|\u6587|Language|noun\n\u6B74|\u6B74|Experience|verb\n\u4EE3|\u4EE3\n\u7D22|\u7D22|Search|verb\n\u691C|\u6AA2\n\u7D50|\u7D50\n\u679C|\u679C|Fruit|noun\n\u5165|\u5165\n\u5148|\u5148\n\u5185|\u5167\n\u4EE5|\u4EE5\n\u53CE|\u53CE\n\u6982|\u6982\n\u6C42|\u6C42\n\u7D04|\u7D04|noun|Commonly used with \u7EA6 to form \u7EBD\u7EA6(New York)\n\u60C5|\u60C5\n\u5831|\u5831\n\u8B77|\u8B77\n\u4FDD|\u4FDD\n\u78BA|\u78BA\n\u7528|\u7528|Use|Verb\n\u9001|\u9001\n\u53D7|\u53D7\n\u5FC5|\u5FC5\n\u7BB1|\u7BB1\n\u8AAC|\u8AAC\n\u7BA1|\u7BA1\n\u9589|\u9589\n\u737B|\u737B\n\u8457|\u8457\n\u5167|\u5167\n\u5316|\u5316\n\u5F37|\u5F37|Strong|adjective\n\u8A18|\u8A18|Remember|verb\n\u61B6|\u61B6|Memory|noun\n\u5FA9|\u5FA9\n\u6821|\u6821\n\u5916|\u5916\n\u559C|\u559C\n\u97F3|\u97F3\n\u542C|\u807D\n\u6B22|\u6B61\n\u97E9|\u97D3\n\u4E50|\u6A02\n\u807D|\u807D\n\u6B61|\u6B61\n\u97D3|\u97D3\n\u6A02|\u6A02\n\u884C|\u884C\n\u98DF|\u98DF\n\u7D1A|\u7D1A\n\u98DB|\u98DB\n\u7248|\u7248\n\u901A|\u901A\n\u5F71|\u5F71\n\u89C6|\u8996\n\u4E0A|\u4E0A|up; on; on top|preposition|Can be used with the word \u65E9 to make \u65E9\u4E0A (morning). Literally: \"upon (the time when it is) early\".\n\u7F51|\u7DB2\n\u624B|\u624B\n\u5E72|\u5E79\n\u62A5|\u5831\n\u673A|\u6A5F|Machine|noun\n\u7535|\u96FB|Electricity|noun|Can form a compound word to form \u7535\u8BDD (telephone)\n\u7EB8|\u7D19|Paper|noun\n\u95FB|\u805E\n\u4F53|\u9AD4\n\u80B2|\u80B2\n\u76EE|\u76EE\n\u54EA|\u54EA|Which/Where|Interrogative particle\n\u91CC|\u88E1\n\u73ED|\u73ED|Class|noun\n\u6C49|\u6F22\n\u54E5|\u54E5\n\u8BED|\u8A9E|Language|noun\n\u8996|\u8996|Visual|noun\n\u7DB2|\u7DB2|Network|noun\n\u5E79|\u5E79|Do|verb\n\u6A5F|\u6A5F|Machine|noun\n\u96FB|\u96FB|Electricity|noun|Can form a compound word to form \u7535\u8BDD (telephone)\n\u7D19|\u7D19\n\u805E|\u805E\n\u9AD4|\u9AD4\n\u88E1|\u88E1\n\u6F22|\u6F22\n\u7968|\u7968\n\u8FDC|\u9060\n\u529E|\u8FA6\n\u8D70|\u8D70\n\u9760|\u9760\n\u5230|\u5230\n\u8FD1|\u8FD1|Near|adjective\n\u8FDF|\u9072|Late|adjective\n\u600E|\u600E|How|interrogative\n\u8F66|\u8ECA|Car|noun\n\u4ECE|\u5F9E|From|preposition\n\u5E97|\u5E97|Shop|Noun\n\u6E2F|\u6E2F|Harbour|Noun|Commonly used with \u9999 (Fragrant) to form \u9999\u6E2F (Hong Kong. Literally: fragrant harbour)\n\u5F00|\u958B|Open|Verb\n\u98DE|\u98DB|Fly|Verb\n\u6566|\u6566|Honest|adjective|Commonly used with \u4F26 to form \u4F26\u6566 (London)\n\u8BA9|\u8B93\n\u975E|\u975E\n\u4F26|\u502B|relationship|noun|Commonly used with \u6566 to form \u4F26\u6566 (London)\n\u573A|\u5834\n\u505C|\u505C|Stop|verb\n\u79BB|\u96E2|Leave|verb\n\u9999|\u9999|Fragrant|adjective|Commonly used with \u6E2F (Harbour) to form \u9999\u6E2F (Hong Kong. Literally: fragrant harbour)\n\u592A|\u592A\n\u5E38|\u5E38\n\u9060|\u9060\n\u8FA6|\u8FA6\n\u9072|\u9072\n\u8ECA|\u8ECA\n\u5F9E|\u5F9E\n\u958B|\u958B\n\u502B|\u502B|relationship|noun|Commonly used with \u6566 to form \u4F26\u6566 (London)\n\u8B93|\u8B93\n\u96E2|\u96E2\n\u4F46|\u4F46\n\u7136|\u7136\n\u957F|\u9577\n\u77ED|\u77ED\n\u867D|\u96D6\n\u989C|\u984F\n\u7EA2|\u7D05\n\u8272|\u8272\n\u7A7F|\u7A7F\n\u6D17|\u6D17\n\u9EC4|\u9EC3\n\u8868|\u8868\n\u5356|\u8CE3\n\u9EC3|\u9EC3\n\u5B50|\u5B50\n\u7D2B|\u7D2B|Purple|noun\n\u5E3D|\u5E3D\n\u4F1A|\u6703\n\u4ECA|\u4ECA|Now|Adverb|Can be combined with \u5929 (day) to make the compound word \u4ECA\u5929(today)\n\u9971|\u98FD\n\u9577|\u9577\n\u96D6|\u96D6\n\u984F|\u984F\n\u7D05|\u7D05\n\u8CE3|\u8CE3\n\u6703|\u6703\n\u98FD|\u98FD\n\u5B9A|\u5B9A\n\u53EF|\u53EF\n\u8BF7|\u8ACB\n\u8DDF|\u8DDF\n\u8D77|\u8D77\n\u8ACB|\u8ACB\n\u4E70|\u8CB7\n\u8CB7|\u8CB7\n\u8FD9|\u9019|This|pronoun\n\u9019|\u9019|This|pronoun\n\u8D35|\u8CB4\n\u8CB4|\u8CB4\n\u6C7D|\u6C7D\n\u8FB9|\u908A\n\u53F3|\u53F3\n\u908A|\u908A\n\u53C8|\u53C8\n\u5199|\u5BEB\n\u53F0|\u53F0|Station|noun|Commonly used with \u6E7E to form \u53F0\u6E7E(Taiwan)\n\u5BEB|\u5BEB|Write|verb\n\u7259|\u7259\n\u62CD|\u62CD\n\u775B|\u775B\n\u53F8|\u53F8\n\u516C|\u516C\n\u5047|\u5047\n\u665A|\u665A\n\u51E0|\u5E7E\n\u81F4|\u81F4\n\u505A|\u505A\n\u5E7E|\u5E7E\n\u5DF2|\u5DF2\n\u7ECF|\u7D93\n\u7D93|\u7D93\n\u525B|\u525B\n\u6C92|\u6C92\n\u8F9B|\u8F9B\n\u82E6|\u82E6\n\u8B1D|\u8B1D|Thank|verb|Can be doubled to say \u8B1D\u8B1D (Thank you)\n\u5E6B|\u5E6B\n\u4F7F|\u4F7F\n\u952E|\u9375\n\u9375|\u9375\n\u80FD|\u80FD\n\u6B4C|\u6B4C\n\u770B|\u770B\n\u5531|\u5531\n\u8DF3|\u8DF3\n\u821E|\u821E\n\u5F0F|\u5F0F\n\u7231|\u611B|Love|noun\n\u7231|\u611B|Love|noun\n\u8C01|\u8AB0|Who|pronoun\n\u50CF|\u50CF\n\u90A3|\u90A3|That|pronoun\n\u4E9B|\u4E9B\n\u8AB0|\u8AB0|Who|pronoun\n\u5171|\u5171\n\u5973|\u5973\n\u670B|\u670B\n\u4EAE|\u4EAE\n\u53CB|\u53CB\n\u6F02|\u6F02\n\u6E38|\u904A\n\u904A|\u904A\n\u7238|\u7238|Dad|particle noun|doubles to make \u7238\u7238(Dad)\n\u4F4F|\u4F4F|live|verb\n\u62FF|\u62FF|take|verb|Can also be used in a compound to form the word \u52A0\u62FF\u5927 (Canada)\n\u7EA6|\u7D04|approximately|adjective|Commonly used with \u7EA6 to form \u7EBD\u7EA6(New York)\n\u6E7E|\u7063|Bay|noun|Commonly used with \u53F0 to form \u53F0\u6E7E(Taiwan)\n\u7063|\u7063|Bay|noun|Commonly used with \u53F0 to form \u53F0\u7063(Taiwan)\n\u5348|\u5348\n\u4E79|\u4E79\n\u4E7E|\u4E7E\n\u4E81|\u4E81\n\u90FD|\u90FD|All/Both|Predeterminer, determiner and pronoun\n\u89C2|\u89C0\n\u89C0|\u89C0\n\u672B|\u672B\n\u592B|\u592B\n\u8072|\u8072\n\u58F0|\u8072\n\u601D|\u601D\n\u6389|\u6389\n\u7ED9|\u7D66\n\u53EA|\u53EA\n\u95EE|\u554F\n\u91CF|\u91CF\n\u7A2E|\u7A2E\n\u5339|\u5339\n\u5DE5|\u5DE5\n\u7D66|\u7D66\n\u676F|\u676F\n\u5976|\u5976\n\u725B|\u725B\n\u642C|\u642C\n\u7B97|\u7B97\n\u6253|\u6253\n\u7A81|\u7A81\n\u91CD|\u91CD\n\u8BAE|\u8B70\n\u623F|\u623F\n\u95F4|\u9593\n\u653E|\u653E\n\u628A|\u628A\n\u8FDB|\u9032\n\u83DC|\u83DC\n\u51AC|\u51AC\n\u6CA1|\u6C92\n\u7537|\u7537\n\u5DEE|\u5DEE\n\u523B|\u523B\n\u7B11|\u7B11\n\u522B|\u5225\n\u8F86|\u8F1B\n\u79DF|\u79DF\n\u51FA|\u51FA\n\u7167|\u7167\n\u611F|\u611F\n\u5192|\u5192\n\u81DF|\u81DF\n\u8B70|\u8B70\n\u9032|\u9032\n\u5225|\u5225\n\u8F1B|\u8F1B\n\u53F7|\u865F|Name|noun|Can form compounds such as \u7801\u53F7 (names of numbers)\n\u865F|\u865F|Name|noun|Can form compounds such as \u7801\u53F7 (names of numbers)\n\u5173|\u95DC\n\u95DC|\u95DC\n\u7801|\u78BC|Number/Numeric|noun/verb|Can form compounds such as \u7801\u53F7 (names of numbers) and \u7535\u8BDD\u7801 (telephone number)\n\u5C11|\u5C11\n\u78BC|\u78BC|Number/Numeric|noun/verb|Can form compounds such as \u7801\u53F7 (names of numbers) and \u7535\u8BDD\u7801 (telephone number)\n\u73A9|\u73A9\n\u620F|\u6232\n\u6232|\u6232\n\u5DE6|\u5DE6\n\u9053|\u9053\n\u5206|\u5206\n\u77E5|\u77E5\n\u8BFB|\u8B80\n\u9605|\u95B1\n\u6D88|\u6D88\n\u606F|\u606F\n\u8FD0|\u904B\n\u52A8|\u52D5\n\u8B80|\u8B80\n\u95B1|\u95B1\n\u904B|\u904B\n\u81FA|\u81FA\n\u9AA8|\u9AA8\n\u5957|\u5957\n\u773C|\u773C\n\u7586|\u7586\n\u52A0|\u52A0|Add|noun|Can also be used in a compound to form the word \u52A0\u62FF\u5927 (Canada)\n\u5761|\u5761\n\u5BF9|\u5C0D|Correct|adjective\n\u51B3|\u6C7A\n\u5C0D|\u5C0D|Correct|adjective\n\u7EFF|\u7DA0\n\u59D0|\u59D0\n\u53D1|\u767C\n\u61C2|\u61C2\n\u732B|\u8C93\n\u718A|\u718A\n\u5934|\u982D\n\u997C|\u9905\n\u5B8C|\u5B8C\n\u8F6C|\u8F49\n\u8C22|\u8B1D|\u8B1D|Thank|verb|Can be doubled to say \u8C22\u8C22 (Thank you)\n\u554A|\u554A\n\u5305|\u5305\n\u9700|\u9700\n\u836F|\u85E5\n\u9662|\u9662\n\u51C0|\u6DE8\n\u732A|\u8C6C\n\u8089|\u8089\n\u6025|\u6025\n\u7DA0|\u7DA0\n\u767C|\u767C\n\u8C93|\u8C93\n\u8F49|\u8F49\n\u85E5|\u85E5\n\u6DE8|\u6DE8\n\u8C6C|\u8C6C\n\u5BA4|\u5BA4\n\u6559|\u6559\n\u540C|\u540C\n\u9898|\u984C\n\u7B14|\u7B46\n\u51C6|\u6E96\n\u79D1|\u79D1\n\u5907|\u5099\n\u8BDD|\u8A71|Speak/Language|verb/noun|Can form a compound word to form \u7535\u8BDD (telephone)\n\u7B46|\u7B46\n\u6E96|\u6E96\n\u5099|\u5099\n\u8A71|\u8A71|Speak/Language|verb/noun|Can form a compound word to form \u7535\u8BDD (telephone)\n\u65C1|\u65C1\n\u5FEB|\u5FEB\n\u5E0C|\u5E0C\n\u5802|\u5802\n\u53C9|\u53C9\n\u6F2B|\u6F2B\n\u6270|\u64FE\n\u64FE|\u64FE\n\u548C|\u548C\n\u982D|\u982D\n\u59CB|\u59CB\n\u65E9|\u65E9|Early|adjective|Can be used with the character \u4E0A to form the compound word \u65E9\u4E0A (morning)\n\u7761|\u7761\n\u5E8A|\u5E8A\n\u89C9|\u89BA\n\u89BA|\u89BA\n\u627E|\u627E\n\u5FD9|\u5FD9|Busy|adjective\n\u6837|\u6A23\n\u6A23|\u6A23\n\u751A|\u751A|What|interrogative adverb\n\u86CB|\u86CB\n\u7897|\u7897\n\u56BF|\u56BF\n\u7092|\u7092\n\u9813|\u9813\n\u67D2|\u67D2\n\u7EBD|\u7D10|Button|noun|Commonly used with \u7EA6 to form \u7EBD\u7EA6(New York)\n\u7D10|\u7D10|Button|noun|Commonly used with \u7EA6 to form \u7EBD\u7EA6(New York)\n\u60C7|\u60C7\n\u679D|\u679D\n\u5BA2|\u5BA2|Guest|noun|Commonly used with \u6C14 to form \u5BA2\u6C14(polite) or \u4E0D\u5BA2\u6C14(You're welcome, literally:  don't be so polite)\n\u6C14|\u6C23|Air|noun|Commonly used with \u5BA2 to form \u5BA2\u6C14(polite) or \u4E0D\u5BA2\u6C14(You're welcome, literally:  don't be so polite)\n\u7CFB|\u7CFB\n\u6C23|\u6C23|Air|noun|Commonly used with \u5BA2 to form \u5BA2\u6C14(polite) or \u4E0D\u5BA2\u6C14(You're welcome, literally:  don't be so polite)\n\u8FCE|\u8FCE\n\u535A|\u535A\n\u61BE|\u61BE\n\u9057|\u907A\n\u907A|\u907A\n\u4FC2|\u4FC2\n\u7E6B|\u7E6B\n\u62B1|\u62B1\n\u6B49|\u6B49\n\u5F1F|\u5F1F\n\u59B9|\u59B9\n\u4E24|\u5169\n\u5F1F|\u5F1F\n\u5169|\u5169\n\u66F4|\u66F4\n\u65E7|\u820A\n\u820A|\u820A\n\u52A9|\u52A9\n\u52A9|\u52A9\n\u5E2E|\u5E6B\n\u5E2E|\u5E6B\n\u8BF4|\u8AAA\n\u8AAA|\u8AAA\n\u63D0|\u63D0\n\u608C|\u608C\n\u6258|\u6258\n\u8D8A|\u8D8A\n\u65F6|\u6642\n\u62DC|\u62DC\n\u904D|\u904D\n\u5144|\u5144\n\u8F15|\u8F15\n\u8BB2|\u8B1B\n\u8B1B|\u8B1B\n\u4E45|\u4E45\n\u613F|\u9858\n\u9858|\u9858\n\u591C|\u591C\n\u6BB5|\u6BB5\n\u70ED|\u71B1\n\u51B0|\u51B0\n\u71B1|\u71B1\n\u7ED3|\u7D50\n\u6784|\u69CB\n\u8FDE|\u9023\n\u69CB|\u69CB\n\u9023|\u9023\n\u996E|\u98F2\n\u6599|\u6599\n\u98F2|\u98F2\n\u96BB|\u96BB\n\u59CA|\u59CA\n\u534A|\u534A\n\u5B69|\u5B69\n\u5C81|\u6B72\n\u6B72|\u6B72\n\u72D7|\u72D7\n\u59BB|\u59BB\n\u4E08|\u4E08\n\u4F4D|\u4F4D\n\u7B49|\u7B49\n\u7235|\u7235\n\u65A4|\u65A4\n\u74F6|\u74F6\n\u6162|\u6162\n\u5582|\u9935\n\u9935|\u9935\n\u77EE|\u77EE\n\u6628|\u6628\n\u8BC9|\u8A34\n\u544A|\u544A\n\u82F9|\u860B\n\u4E1A|\u696D\n\u8A34|\u8A34\n\u860B|\u860B\n\u5F80|\u5F80\n\u5750|\u5750\n\u8111|\u8166\n\u8166|\u8166\n\u4F1E|\u5098\n\u5B9C|\u5B9C\n\u4FBF|\u4FBF\n\u5098|\u5098\n\u88E4|\u8932\n\u8932|\u8932\n\u4E3A|\u70BA\n\u4E3A|\u70BA\n\u70BA|\u70BA\n\u8BA2|\u8A02\n\u8212|\u8212\n\u8A02|\u8A02\n\u7A7A|\u7A7A\n\u7559|\u7559\n\u9A91|\u9A0E\n\u8D85|\u8D85\n\u9A0E|\u9A0E\n\u75C5|\u75C5\n\u75C5|\u75C5\n\u75C5|\u75C5\n\u7D2F|\u7D2F\n\u4F11|\u4F11\n\u8C46|\u8C46\n\u62C9|\u62C9\n\u6D46|\u6F3F\n\u7CA5|\u7CA5\n\u62C9|\u62C9\n\u7B3C|\u7C60\n\u6F3F|\u6F3F\n\u7C60|\u7C60\n\u91D1|\u91D1\n\u5361|\u5361\n\u94B1|\u9322\n\u6BDB|\u6BDB\n\u5343|\u5343\n\u6536|\u6536\n\u4E07|\u842C\n\u842C|\u842C\n\u9322|\u9322".split('\n');
var characters = charactersNoDef.concat(charactersWithDef);
exports.characters = characters;
var charactersVersion = 303;
exports.charactersVersion = charactersVersion;
},{}],"cache.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Cache = void 0;

var _characters = require("./characters.js");

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance"); }

function _iterableToArrayLimit(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

var Cache =
/*#__PURE__*/
function () {
  function Cache() {
    _classCallCheck(this, Cache);
  }

  _createClass(Cache, null, [{
    key: "setCharacter",
    value: function setCharacter(simplifiedCharacter, realCharacter) {
      localStorage.setItem(simplifiedCharacter, realCharacter);
    }
  }, {
    key: "updateLocalStorage",
    value: function updateLocalStorage() {
      if (localStorage.getItem("version") == null || parseFloat(localStorage.getItem("version")) < _characters.charactersVersion) {
        localStorage.clear();
        console.log("clear cache");
        localStorage.setItem("version", _characters.charactersVersion);

        _characters.characters.forEach(function (x) {
          var _x$split = x.split("|"),
              _x$split2 = _slicedToArray(_x$split, 2),
              simplified = _x$split2[0],
              traditional = _x$split2[1];

          localStorage.setItem(simplified, traditional);
        });
      }
    }
  }]);

  return Cache;
}();

exports.Cache = Cache;
},{"./characters.js":"characters.js"}],"Dictionary.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _characters = require("./characters.js");

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance"); }

function _iterableToArrayLimit(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

var parsedData;

var parseDataIfNeeded = function parseDataIfNeeded() {
  if (parsedData) return;
  parsedData = Object.create(null);

  for (var char in _characters.characters) {
    var _characters$char$spli = _characters.characters[char].split("|"),
        _characters$char$spli2 = _slicedToArray(_characters$char$spli, 5),
        simplified = _characters$char$spli2[0],
        traditional = _characters$char$spli2[1],
        meaning = _characters$char$spli2[2],
        type = _characters$char$spli2[3],
        explanation = _characters$char$spli2[4];

    parsedData[simplified] = {
      traditional: traditional,
      meaning: meaning,
      type: type,
      explanation: explanation
    };
  }
};

var Dictionary =
/*#__PURE__*/
function () {
  function Dictionary() {
    _classCallCheck(this, Dictionary);

    parseDataIfNeeded();
  }

  _createClass(Dictionary, [{
    key: "translateToTraditional",
    value: function translateToTraditional(simplified) {
      return parsedData[simplified].traditional;
    }
  }]);

  return Dictionary;
}();

exports.default = Dictionary;
},{"./characters.js":"characters.js"}],"../../node_modules/sizzle/dist/sizzle.js":[function(require,module,exports) {
var define;
/*!
 * Sizzle CSS Selector Engine v2.3.3
 * https://sizzlejs.com/
 *
 * Copyright jQuery Foundation and other contributors
 * Released under the MIT license
 * http://jquery.org/license
 *
 * Date: 2016-08-08
 */
(function( window ) {

var i,
	support,
	Expr,
	getText,
	isXML,
	tokenize,
	compile,
	select,
	outermostContext,
	sortInput,
	hasDuplicate,

	// Local document vars
	setDocument,
	document,
	docElem,
	documentIsHTML,
	rbuggyQSA,
	rbuggyMatches,
	matches,
	contains,

	// Instance-specific data
	expando = "sizzle" + 1 * new Date(),
	preferredDoc = window.document,
	dirruns = 0,
	done = 0,
	classCache = createCache(),
	tokenCache = createCache(),
	compilerCache = createCache(),
	sortOrder = function( a, b ) {
		if ( a === b ) {
			hasDuplicate = true;
		}
		return 0;
	},

	// Instance methods
	hasOwn = ({}).hasOwnProperty,
	arr = [],
	pop = arr.pop,
	push_native = arr.push,
	push = arr.push,
	slice = arr.slice,
	// Use a stripped-down indexOf as it's faster than native
	// https://jsperf.com/thor-indexof-vs-for/5
	indexOf = function( list, elem ) {
		var i = 0,
			len = list.length;
		for ( ; i < len; i++ ) {
			if ( list[i] === elem ) {
				return i;
			}
		}
		return -1;
	},

	booleans = "checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",

	// Regular expressions

	// http://www.w3.org/TR/css3-selectors/#whitespace
	whitespace = "[\\x20\\t\\r\\n\\f]",

	// http://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
	identifier = "(?:\\\\.|[\\w-]|[^\0-\\xa0])+",

	// Attribute selectors: http://www.w3.org/TR/selectors/#attribute-selectors
	attributes = "\\[" + whitespace + "*(" + identifier + ")(?:" + whitespace +
		// Operator (capture 2)
		"*([*^$|!~]?=)" + whitespace +
		// "Attribute values must be CSS identifiers [capture 5] or strings [capture 3 or capture 4]"
		"*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|(" + identifier + "))|)" + whitespace +
		"*\\]",

	pseudos = ":(" + identifier + ")(?:\\((" +
		// To reduce the number of selectors needing tokenize in the preFilter, prefer arguments:
		// 1. quoted (capture 3; capture 4 or capture 5)
		"('((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\")|" +
		// 2. simple (capture 6)
		"((?:\\\\.|[^\\\\()[\\]]|" + attributes + ")*)|" +
		// 3. anything else (capture 2)
		".*" +
		")\\)|)",

	// Leading and non-escaped trailing whitespace, capturing some non-whitespace characters preceding the latter
	rwhitespace = new RegExp( whitespace + "+", "g" ),
	rtrim = new RegExp( "^" + whitespace + "+|((?:^|[^\\\\])(?:\\\\.)*)" + whitespace + "+$", "g" ),

	rcomma = new RegExp( "^" + whitespace + "*," + whitespace + "*" ),
	rcombinators = new RegExp( "^" + whitespace + "*([>+~]|" + whitespace + ")" + whitespace + "*" ),

	rattributeQuotes = new RegExp( "=" + whitespace + "*([^\\]'\"]*?)" + whitespace + "*\\]", "g" ),

	rpseudo = new RegExp( pseudos ),
	ridentifier = new RegExp( "^" + identifier + "$" ),

	matchExpr = {
		"ID": new RegExp( "^#(" + identifier + ")" ),
		"CLASS": new RegExp( "^\\.(" + identifier + ")" ),
		"TAG": new RegExp( "^(" + identifier + "|[*])" ),
		"ATTR": new RegExp( "^" + attributes ),
		"PSEUDO": new RegExp( "^" + pseudos ),
		"CHILD": new RegExp( "^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\(" + whitespace +
			"*(even|odd|(([+-]|)(\\d*)n|)" + whitespace + "*(?:([+-]|)" + whitespace +
			"*(\\d+)|))" + whitespace + "*\\)|)", "i" ),
		"bool": new RegExp( "^(?:" + booleans + ")$", "i" ),
		// For use in libraries implementing .is()
		// We use this for POS matching in `select`
		"needsContext": new RegExp( "^" + whitespace + "*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\(" +
			whitespace + "*((?:-\\d)?\\d*)" + whitespace + "*\\)|)(?=[^-]|$)", "i" )
	},

	rinputs = /^(?:input|select|textarea|button)$/i,
	rheader = /^h\d$/i,

	rnative = /^[^{]+\{\s*\[native \w/,

	// Easily-parseable/retrievable ID or TAG or CLASS selectors
	rquickExpr = /^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,

	rsibling = /[+~]/,

	// CSS escapes
	// http://www.w3.org/TR/CSS21/syndata.html#escaped-characters
	runescape = new RegExp( "\\\\([\\da-f]{1,6}" + whitespace + "?|(" + whitespace + ")|.)", "ig" ),
	funescape = function( _, escaped, escapedWhitespace ) {
		var high = "0x" + escaped - 0x10000;
		// NaN means non-codepoint
		// Support: Firefox<24
		// Workaround erroneous numeric interpretation of +"0x"
		return high !== high || escapedWhitespace ?
			escaped :
			high < 0 ?
				// BMP codepoint
				String.fromCharCode( high + 0x10000 ) :
				// Supplemental Plane codepoint (surrogate pair)
				String.fromCharCode( high >> 10 | 0xD800, high & 0x3FF | 0xDC00 );
	},

	// CSS string/identifier serialization
	// https://drafts.csswg.org/cssom/#common-serializing-idioms
	rcssescape = /([\0-\x1f\x7f]|^-?\d)|^-$|[^\0-\x1f\x7f-\uFFFF\w-]/g,
	fcssescape = function( ch, asCodePoint ) {
		if ( asCodePoint ) {

			// U+0000 NULL becomes U+FFFD REPLACEMENT CHARACTER
			if ( ch === "\0" ) {
				return "\uFFFD";
			}

			// Control characters and (dependent upon position) numbers get escaped as code points
			return ch.slice( 0, -1 ) + "\\" + ch.charCodeAt( ch.length - 1 ).toString( 16 ) + " ";
		}

		// Other potentially-special ASCII characters get backslash-escaped
		return "\\" + ch;
	},

	// Used for iframes
	// See setDocument()
	// Removing the function wrapper causes a "Permission Denied"
	// error in IE
	unloadHandler = function() {
		setDocument();
	},

	disabledAncestor = addCombinator(
		function( elem ) {
			return elem.disabled === true && ("form" in elem || "label" in elem);
		},
		{ dir: "parentNode", next: "legend" }
	);

// Optimize for push.apply( _, NodeList )
try {
	push.apply(
		(arr = slice.call( preferredDoc.childNodes )),
		preferredDoc.childNodes
	);
	// Support: Android<4.0
	// Detect silently failing push.apply
	arr[ preferredDoc.childNodes.length ].nodeType;
} catch ( e ) {
	push = { apply: arr.length ?

		// Leverage slice if possible
		function( target, els ) {
			push_native.apply( target, slice.call(els) );
		} :

		// Support: IE<9
		// Otherwise append directly
		function( target, els ) {
			var j = target.length,
				i = 0;
			// Can't trust NodeList.length
			while ( (target[j++] = els[i++]) ) {}
			target.length = j - 1;
		}
	};
}

function Sizzle( selector, context, results, seed ) {
	var m, i, elem, nid, match, groups, newSelector,
		newContext = context && context.ownerDocument,

		// nodeType defaults to 9, since context defaults to document
		nodeType = context ? context.nodeType : 9;

	results = results || [];

	// Return early from calls with invalid selector or context
	if ( typeof selector !== "string" || !selector ||
		nodeType !== 1 && nodeType !== 9 && nodeType !== 11 ) {

		return results;
	}

	// Try to shortcut find operations (as opposed to filters) in HTML documents
	if ( !seed ) {

		if ( ( context ? context.ownerDocument || context : preferredDoc ) !== document ) {
			setDocument( context );
		}
		context = context || document;

		if ( documentIsHTML ) {

			// If the selector is sufficiently simple, try using a "get*By*" DOM method
			// (excepting DocumentFragment context, where the methods don't exist)
			if ( nodeType !== 11 && (match = rquickExpr.exec( selector )) ) {

				// ID selector
				if ( (m = match[1]) ) {

					// Document context
					if ( nodeType === 9 ) {
						if ( (elem = context.getElementById( m )) ) {

							// Support: IE, Opera, Webkit
							// TODO: identify versions
							// getElementById can match elements by name instead of ID
							if ( elem.id === m ) {
								results.push( elem );
								return results;
							}
						} else {
							return results;
						}

					// Element context
					} else {

						// Support: IE, Opera, Webkit
						// TODO: identify versions
						// getElementById can match elements by name instead of ID
						if ( newContext && (elem = newContext.getElementById( m )) &&
							contains( context, elem ) &&
							elem.id === m ) {

							results.push( elem );
							return results;
						}
					}

				// Type selector
				} else if ( match[2] ) {
					push.apply( results, context.getElementsByTagName( selector ) );
					return results;

				// Class selector
				} else if ( (m = match[3]) && support.getElementsByClassName &&
					context.getElementsByClassName ) {

					push.apply( results, context.getElementsByClassName( m ) );
					return results;
				}
			}

			// Take advantage of querySelectorAll
			if ( support.qsa &&
				!compilerCache[ selector + " " ] &&
				(!rbuggyQSA || !rbuggyQSA.test( selector )) ) {

				if ( nodeType !== 1 ) {
					newContext = context;
					newSelector = selector;

				// qSA looks outside Element context, which is not what we want
				// Thanks to Andrew Dupont for this workaround technique
				// Support: IE <=8
				// Exclude object elements
				} else if ( context.nodeName.toLowerCase() !== "object" ) {

					// Capture the context ID, setting it first if necessary
					if ( (nid = context.getAttribute( "id" )) ) {
						nid = nid.replace( rcssescape, fcssescape );
					} else {
						context.setAttribute( "id", (nid = expando) );
					}

					// Prefix every selector in the list
					groups = tokenize( selector );
					i = groups.length;
					while ( i-- ) {
						groups[i] = "#" + nid + " " + toSelector( groups[i] );
					}
					newSelector = groups.join( "," );

					// Expand context for sibling selectors
					newContext = rsibling.test( selector ) && testContext( context.parentNode ) ||
						context;
				}

				if ( newSelector ) {
					try {
						push.apply( results,
							newContext.querySelectorAll( newSelector )
						);
						return results;
					} catch ( qsaError ) {
					} finally {
						if ( nid === expando ) {
							context.removeAttribute( "id" );
						}
					}
				}
			}
		}
	}

	// All others
	return select( selector.replace( rtrim, "$1" ), context, results, seed );
}

/**
 * Create key-value caches of limited size
 * @returns {function(string, object)} Returns the Object data after storing it on itself with
 *	property name the (space-suffixed) string and (if the cache is larger than Expr.cacheLength)
 *	deleting the oldest entry
 */
function createCache() {
	var keys = [];

	function cache( key, value ) {
		// Use (key + " ") to avoid collision with native prototype properties (see Issue #157)
		if ( keys.push( key + " " ) > Expr.cacheLength ) {
			// Only keep the most recent entries
			delete cache[ keys.shift() ];
		}
		return (cache[ key + " " ] = value);
	}
	return cache;
}

/**
 * Mark a function for special use by Sizzle
 * @param {Function} fn The function to mark
 */
function markFunction( fn ) {
	fn[ expando ] = true;
	return fn;
}

/**
 * Support testing using an element
 * @param {Function} fn Passed the created element and returns a boolean result
 */
function assert( fn ) {
	var el = document.createElement("fieldset");

	try {
		return !!fn( el );
	} catch (e) {
		return false;
	} finally {
		// Remove from its parent by default
		if ( el.parentNode ) {
			el.parentNode.removeChild( el );
		}
		// release memory in IE
		el = null;
	}
}

/**
 * Adds the same handler for all of the specified attrs
 * @param {String} attrs Pipe-separated list of attributes
 * @param {Function} handler The method that will be applied
 */
function addHandle( attrs, handler ) {
	var arr = attrs.split("|"),
		i = arr.length;

	while ( i-- ) {
		Expr.attrHandle[ arr[i] ] = handler;
	}
}

/**
 * Checks document order of two siblings
 * @param {Element} a
 * @param {Element} b
 * @returns {Number} Returns less than 0 if a precedes b, greater than 0 if a follows b
 */
function siblingCheck( a, b ) {
	var cur = b && a,
		diff = cur && a.nodeType === 1 && b.nodeType === 1 &&
			a.sourceIndex - b.sourceIndex;

	// Use IE sourceIndex if available on both nodes
	if ( diff ) {
		return diff;
	}

	// Check if b follows a
	if ( cur ) {
		while ( (cur = cur.nextSibling) ) {
			if ( cur === b ) {
				return -1;
			}
		}
	}

	return a ? 1 : -1;
}

/**
 * Returns a function to use in pseudos for input types
 * @param {String} type
 */
function createInputPseudo( type ) {
	return function( elem ) {
		var name = elem.nodeName.toLowerCase();
		return name === "input" && elem.type === type;
	};
}

/**
 * Returns a function to use in pseudos for buttons
 * @param {String} type
 */
function createButtonPseudo( type ) {
	return function( elem ) {
		var name = elem.nodeName.toLowerCase();
		return (name === "input" || name === "button") && elem.type === type;
	};
}

/**
 * Returns a function to use in pseudos for :enabled/:disabled
 * @param {Boolean} disabled true for :disabled; false for :enabled
 */
function createDisabledPseudo( disabled ) {

	// Known :disabled false positives: fieldset[disabled] > legend:nth-of-type(n+2) :can-disable
	return function( elem ) {

		// Only certain elements can match :enabled or :disabled
		// https://html.spec.whatwg.org/multipage/scripting.html#selector-enabled
		// https://html.spec.whatwg.org/multipage/scripting.html#selector-disabled
		if ( "form" in elem ) {

			// Check for inherited disabledness on relevant non-disabled elements:
			// * listed form-associated elements in a disabled fieldset
			//   https://html.spec.whatwg.org/multipage/forms.html#category-listed
			//   https://html.spec.whatwg.org/multipage/forms.html#concept-fe-disabled
			// * option elements in a disabled optgroup
			//   https://html.spec.whatwg.org/multipage/forms.html#concept-option-disabled
			// All such elements have a "form" property.
			if ( elem.parentNode && elem.disabled === false ) {

				// Option elements defer to a parent optgroup if present
				if ( "label" in elem ) {
					if ( "label" in elem.parentNode ) {
						return elem.parentNode.disabled === disabled;
					} else {
						return elem.disabled === disabled;
					}
				}

				// Support: IE 6 - 11
				// Use the isDisabled shortcut property to check for disabled fieldset ancestors
				return elem.isDisabled === disabled ||

					// Where there is no isDisabled, check manually
					/* jshint -W018 */
					elem.isDisabled !== !disabled &&
						disabledAncestor( elem ) === disabled;
			}

			return elem.disabled === disabled;

		// Try to winnow out elements that can't be disabled before trusting the disabled property.
		// Some victims get caught in our net (label, legend, menu, track), but it shouldn't
		// even exist on them, let alone have a boolean value.
		} else if ( "label" in elem ) {
			return elem.disabled === disabled;
		}

		// Remaining elements are neither :enabled nor :disabled
		return false;
	};
}

/**
 * Returns a function to use in pseudos for positionals
 * @param {Function} fn
 */
function createPositionalPseudo( fn ) {
	return markFunction(function( argument ) {
		argument = +argument;
		return markFunction(function( seed, matches ) {
			var j,
				matchIndexes = fn( [], seed.length, argument ),
				i = matchIndexes.length;

			// Match elements found at the specified indexes
			while ( i-- ) {
				if ( seed[ (j = matchIndexes[i]) ] ) {
					seed[j] = !(matches[j] = seed[j]);
				}
			}
		});
	});
}

/**
 * Checks a node for validity as a Sizzle context
 * @param {Element|Object=} context
 * @returns {Element|Object|Boolean} The input node if acceptable, otherwise a falsy value
 */
function testContext( context ) {
	return context && typeof context.getElementsByTagName !== "undefined" && context;
}

// Expose support vars for convenience
support = Sizzle.support = {};

/**
 * Detects XML nodes
 * @param {Element|Object} elem An element or a document
 * @returns {Boolean} True iff elem is a non-HTML XML node
 */
isXML = Sizzle.isXML = function( elem ) {
	// documentElement is verified for cases where it doesn't yet exist
	// (such as loading iframes in IE - #4833)
	var documentElement = elem && (elem.ownerDocument || elem).documentElement;
	return documentElement ? documentElement.nodeName !== "HTML" : false;
};

/**
 * Sets document-related variables once based on the current document
 * @param {Element|Object} [doc] An element or document object to use to set the document
 * @returns {Object} Returns the current document
 */
setDocument = Sizzle.setDocument = function( node ) {
	var hasCompare, subWindow,
		doc = node ? node.ownerDocument || node : preferredDoc;

	// Return early if doc is invalid or already selected
	if ( doc === document || doc.nodeType !== 9 || !doc.documentElement ) {
		return document;
	}

	// Update global variables
	document = doc;
	docElem = document.documentElement;
	documentIsHTML = !isXML( document );

	// Support: IE 9-11, Edge
	// Accessing iframe documents after unload throws "permission denied" errors (jQuery #13936)
	if ( preferredDoc !== document &&
		(subWindow = document.defaultView) && subWindow.top !== subWindow ) {

		// Support: IE 11, Edge
		if ( subWindow.addEventListener ) {
			subWindow.addEventListener( "unload", unloadHandler, false );

		// Support: IE 9 - 10 only
		} else if ( subWindow.attachEvent ) {
			subWindow.attachEvent( "onunload", unloadHandler );
		}
	}

	/* Attributes
	---------------------------------------------------------------------- */

	// Support: IE<8
	// Verify that getAttribute really returns attributes and not properties
	// (excepting IE8 booleans)
	support.attributes = assert(function( el ) {
		el.className = "i";
		return !el.getAttribute("className");
	});

	/* getElement(s)By*
	---------------------------------------------------------------------- */

	// Check if getElementsByTagName("*") returns only elements
	support.getElementsByTagName = assert(function( el ) {
		el.appendChild( document.createComment("") );
		return !el.getElementsByTagName("*").length;
	});

	// Support: IE<9
	support.getElementsByClassName = rnative.test( document.getElementsByClassName );

	// Support: IE<10
	// Check if getElementById returns elements by name
	// The broken getElementById methods don't pick up programmatically-set names,
	// so use a roundabout getElementsByName test
	support.getById = assert(function( el ) {
		docElem.appendChild( el ).id = expando;
		return !document.getElementsByName || !document.getElementsByName( expando ).length;
	});

	// ID filter and find
	if ( support.getById ) {
		Expr.filter["ID"] = function( id ) {
			var attrId = id.replace( runescape, funescape );
			return function( elem ) {
				return elem.getAttribute("id") === attrId;
			};
		};
		Expr.find["ID"] = function( id, context ) {
			if ( typeof context.getElementById !== "undefined" && documentIsHTML ) {
				var elem = context.getElementById( id );
				return elem ? [ elem ] : [];
			}
		};
	} else {
		Expr.filter["ID"] =  function( id ) {
			var attrId = id.replace( runescape, funescape );
			return function( elem ) {
				var node = typeof elem.getAttributeNode !== "undefined" &&
					elem.getAttributeNode("id");
				return node && node.value === attrId;
			};
		};

		// Support: IE 6 - 7 only
		// getElementById is not reliable as a find shortcut
		Expr.find["ID"] = function( id, context ) {
			if ( typeof context.getElementById !== "undefined" && documentIsHTML ) {
				var node, i, elems,
					elem = context.getElementById( id );

				if ( elem ) {

					// Verify the id attribute
					node = elem.getAttributeNode("id");
					if ( node && node.value === id ) {
						return [ elem ];
					}

					// Fall back on getElementsByName
					elems = context.getElementsByName( id );
					i = 0;
					while ( (elem = elems[i++]) ) {
						node = elem.getAttributeNode("id");
						if ( node && node.value === id ) {
							return [ elem ];
						}
					}
				}

				return [];
			}
		};
	}

	// Tag
	Expr.find["TAG"] = support.getElementsByTagName ?
		function( tag, context ) {
			if ( typeof context.getElementsByTagName !== "undefined" ) {
				return context.getElementsByTagName( tag );

			// DocumentFragment nodes don't have gEBTN
			} else if ( support.qsa ) {
				return context.querySelectorAll( tag );
			}
		} :

		function( tag, context ) {
			var elem,
				tmp = [],
				i = 0,
				// By happy coincidence, a (broken) gEBTN appears on DocumentFragment nodes too
				results = context.getElementsByTagName( tag );

			// Filter out possible comments
			if ( tag === "*" ) {
				while ( (elem = results[i++]) ) {
					if ( elem.nodeType === 1 ) {
						tmp.push( elem );
					}
				}

				return tmp;
			}
			return results;
		};

	// Class
	Expr.find["CLASS"] = support.getElementsByClassName && function( className, context ) {
		if ( typeof context.getElementsByClassName !== "undefined" && documentIsHTML ) {
			return context.getElementsByClassName( className );
		}
	};

	/* QSA/matchesSelector
	---------------------------------------------------------------------- */

	// QSA and matchesSelector support

	// matchesSelector(:active) reports false when true (IE9/Opera 11.5)
	rbuggyMatches = [];

	// qSa(:focus) reports false when true (Chrome 21)
	// We allow this because of a bug in IE8/9 that throws an error
	// whenever `document.activeElement` is accessed on an iframe
	// So, we allow :focus to pass through QSA all the time to avoid the IE error
	// See https://bugs.jquery.com/ticket/13378
	rbuggyQSA = [];

	if ( (support.qsa = rnative.test( document.querySelectorAll )) ) {
		// Build QSA regex
		// Regex strategy adopted from Diego Perini
		assert(function( el ) {
			// Select is set to empty string on purpose
			// This is to test IE's treatment of not explicitly
			// setting a boolean content attribute,
			// since its presence should be enough
			// https://bugs.jquery.com/ticket/12359
			docElem.appendChild( el ).innerHTML = "<a id='" + expando + "'></a>" +
				"<select id='" + expando + "-\r\\' msallowcapture=''>" +
				"<option selected=''></option></select>";

			// Support: IE8, Opera 11-12.16
			// Nothing should be selected when empty strings follow ^= or $= or *=
			// The test attribute must be unknown in Opera but "safe" for WinRT
			// https://msdn.microsoft.com/en-us/library/ie/hh465388.aspx#attribute_section
			if ( el.querySelectorAll("[msallowcapture^='']").length ) {
				rbuggyQSA.push( "[*^$]=" + whitespace + "*(?:''|\"\")" );
			}

			// Support: IE8
			// Boolean attributes and "value" are not treated correctly
			if ( !el.querySelectorAll("[selected]").length ) {
				rbuggyQSA.push( "\\[" + whitespace + "*(?:value|" + booleans + ")" );
			}

			// Support: Chrome<29, Android<4.4, Safari<7.0+, iOS<7.0+, PhantomJS<1.9.8+
			if ( !el.querySelectorAll( "[id~=" + expando + "-]" ).length ) {
				rbuggyQSA.push("~=");
			}

			// Webkit/Opera - :checked should return selected option elements
			// http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#checked
			// IE8 throws error here and will not see later tests
			if ( !el.querySelectorAll(":checked").length ) {
				rbuggyQSA.push(":checked");
			}

			// Support: Safari 8+, iOS 8+
			// https://bugs.webkit.org/show_bug.cgi?id=136851
			// In-page `selector#id sibling-combinator selector` fails
			if ( !el.querySelectorAll( "a#" + expando + "+*" ).length ) {
				rbuggyQSA.push(".#.+[+~]");
			}
		});

		assert(function( el ) {
			el.innerHTML = "<a href='' disabled='disabled'></a>" +
				"<select disabled='disabled'><option/></select>";

			// Support: Windows 8 Native Apps
			// The type and name attributes are restricted during .innerHTML assignment
			var input = document.createElement("input");
			input.setAttribute( "type", "hidden" );
			el.appendChild( input ).setAttribute( "name", "D" );

			// Support: IE8
			// Enforce case-sensitivity of name attribute
			if ( el.querySelectorAll("[name=d]").length ) {
				rbuggyQSA.push( "name" + whitespace + "*[*^$|!~]?=" );
			}

			// FF 3.5 - :enabled/:disabled and hidden elements (hidden elements are still enabled)
			// IE8 throws error here and will not see later tests
			if ( el.querySelectorAll(":enabled").length !== 2 ) {
				rbuggyQSA.push( ":enabled", ":disabled" );
			}

			// Support: IE9-11+
			// IE's :disabled selector does not pick up the children of disabled fieldsets
			docElem.appendChild( el ).disabled = true;
			if ( el.querySelectorAll(":disabled").length !== 2 ) {
				rbuggyQSA.push( ":enabled", ":disabled" );
			}

			// Opera 10-11 does not throw on post-comma invalid pseudos
			el.querySelectorAll("*,:x");
			rbuggyQSA.push(",.*:");
		});
	}

	if ( (support.matchesSelector = rnative.test( (matches = docElem.matches ||
		docElem.webkitMatchesSelector ||
		docElem.mozMatchesSelector ||
		docElem.oMatchesSelector ||
		docElem.msMatchesSelector) )) ) {

		assert(function( el ) {
			// Check to see if it's possible to do matchesSelector
			// on a disconnected node (IE 9)
			support.disconnectedMatch = matches.call( el, "*" );

			// This should fail with an exception
			// Gecko does not error, returns false instead
			matches.call( el, "[s!='']:x" );
			rbuggyMatches.push( "!=", pseudos );
		});
	}

	rbuggyQSA = rbuggyQSA.length && new RegExp( rbuggyQSA.join("|") );
	rbuggyMatches = rbuggyMatches.length && new RegExp( rbuggyMatches.join("|") );

	/* Contains
	---------------------------------------------------------------------- */
	hasCompare = rnative.test( docElem.compareDocumentPosition );

	// Element contains another
	// Purposefully self-exclusive
	// As in, an element does not contain itself
	contains = hasCompare || rnative.test( docElem.contains ) ?
		function( a, b ) {
			var adown = a.nodeType === 9 ? a.documentElement : a,
				bup = b && b.parentNode;
			return a === bup || !!( bup && bup.nodeType === 1 && (
				adown.contains ?
					adown.contains( bup ) :
					a.compareDocumentPosition && a.compareDocumentPosition( bup ) & 16
			));
		} :
		function( a, b ) {
			if ( b ) {
				while ( (b = b.parentNode) ) {
					if ( b === a ) {
						return true;
					}
				}
			}
			return false;
		};

	/* Sorting
	---------------------------------------------------------------------- */

	// Document order sorting
	sortOrder = hasCompare ?
	function( a, b ) {

		// Flag for duplicate removal
		if ( a === b ) {
			hasDuplicate = true;
			return 0;
		}

		// Sort on method existence if only one input has compareDocumentPosition
		var compare = !a.compareDocumentPosition - !b.compareDocumentPosition;
		if ( compare ) {
			return compare;
		}

		// Calculate position if both inputs belong to the same document
		compare = ( a.ownerDocument || a ) === ( b.ownerDocument || b ) ?
			a.compareDocumentPosition( b ) :

			// Otherwise we know they are disconnected
			1;

		// Disconnected nodes
		if ( compare & 1 ||
			(!support.sortDetached && b.compareDocumentPosition( a ) === compare) ) {

			// Choose the first element that is related to our preferred document
			if ( a === document || a.ownerDocument === preferredDoc && contains(preferredDoc, a) ) {
				return -1;
			}
			if ( b === document || b.ownerDocument === preferredDoc && contains(preferredDoc, b) ) {
				return 1;
			}

			// Maintain original order
			return sortInput ?
				( indexOf( sortInput, a ) - indexOf( sortInput, b ) ) :
				0;
		}

		return compare & 4 ? -1 : 1;
	} :
	function( a, b ) {
		// Exit early if the nodes are identical
		if ( a === b ) {
			hasDuplicate = true;
			return 0;
		}

		var cur,
			i = 0,
			aup = a.parentNode,
			bup = b.parentNode,
			ap = [ a ],
			bp = [ b ];

		// Parentless nodes are either documents or disconnected
		if ( !aup || !bup ) {
			return a === document ? -1 :
				b === document ? 1 :
				aup ? -1 :
				bup ? 1 :
				sortInput ?
				( indexOf( sortInput, a ) - indexOf( sortInput, b ) ) :
				0;

		// If the nodes are siblings, we can do a quick check
		} else if ( aup === bup ) {
			return siblingCheck( a, b );
		}

		// Otherwise we need full lists of their ancestors for comparison
		cur = a;
		while ( (cur = cur.parentNode) ) {
			ap.unshift( cur );
		}
		cur = b;
		while ( (cur = cur.parentNode) ) {
			bp.unshift( cur );
		}

		// Walk down the tree looking for a discrepancy
		while ( ap[i] === bp[i] ) {
			i++;
		}

		return i ?
			// Do a sibling check if the nodes have a common ancestor
			siblingCheck( ap[i], bp[i] ) :

			// Otherwise nodes in our document sort first
			ap[i] === preferredDoc ? -1 :
			bp[i] === preferredDoc ? 1 :
			0;
	};

	return document;
};

Sizzle.matches = function( expr, elements ) {
	return Sizzle( expr, null, null, elements );
};

Sizzle.matchesSelector = function( elem, expr ) {
	// Set document vars if needed
	if ( ( elem.ownerDocument || elem ) !== document ) {
		setDocument( elem );
	}

	// Make sure that attribute selectors are quoted
	expr = expr.replace( rattributeQuotes, "='$1']" );

	if ( support.matchesSelector && documentIsHTML &&
		!compilerCache[ expr + " " ] &&
		( !rbuggyMatches || !rbuggyMatches.test( expr ) ) &&
		( !rbuggyQSA     || !rbuggyQSA.test( expr ) ) ) {

		try {
			var ret = matches.call( elem, expr );

			// IE 9's matchesSelector returns false on disconnected nodes
			if ( ret || support.disconnectedMatch ||
					// As well, disconnected nodes are said to be in a document
					// fragment in IE 9
					elem.document && elem.document.nodeType !== 11 ) {
				return ret;
			}
		} catch (e) {}
	}

	return Sizzle( expr, document, null, [ elem ] ).length > 0;
};

Sizzle.contains = function( context, elem ) {
	// Set document vars if needed
	if ( ( context.ownerDocument || context ) !== document ) {
		setDocument( context );
	}
	return contains( context, elem );
};

Sizzle.attr = function( elem, name ) {
	// Set document vars if needed
	if ( ( elem.ownerDocument || elem ) !== document ) {
		setDocument( elem );
	}

	var fn = Expr.attrHandle[ name.toLowerCase() ],
		// Don't get fooled by Object.prototype properties (jQuery #13807)
		val = fn && hasOwn.call( Expr.attrHandle, name.toLowerCase() ) ?
			fn( elem, name, !documentIsHTML ) :
			undefined;

	return val !== undefined ?
		val :
		support.attributes || !documentIsHTML ?
			elem.getAttribute( name ) :
			(val = elem.getAttributeNode(name)) && val.specified ?
				val.value :
				null;
};

Sizzle.escape = function( sel ) {
	return (sel + "").replace( rcssescape, fcssescape );
};

Sizzle.error = function( msg ) {
	throw new Error( "Syntax error, unrecognized expression: " + msg );
};

/**
 * Document sorting and removing duplicates
 * @param {ArrayLike} results
 */
Sizzle.uniqueSort = function( results ) {
	var elem,
		duplicates = [],
		j = 0,
		i = 0;

	// Unless we *know* we can detect duplicates, assume their presence
	hasDuplicate = !support.detectDuplicates;
	sortInput = !support.sortStable && results.slice( 0 );
	results.sort( sortOrder );

	if ( hasDuplicate ) {
		while ( (elem = results[i++]) ) {
			if ( elem === results[ i ] ) {
				j = duplicates.push( i );
			}
		}
		while ( j-- ) {
			results.splice( duplicates[ j ], 1 );
		}
	}

	// Clear input after sorting to release objects
	// See https://github.com/jquery/sizzle/pull/225
	sortInput = null;

	return results;
};

/**
 * Utility function for retrieving the text value of an array of DOM nodes
 * @param {Array|Element} elem
 */
getText = Sizzle.getText = function( elem ) {
	var node,
		ret = "",
		i = 0,
		nodeType = elem.nodeType;

	if ( !nodeType ) {
		// If no nodeType, this is expected to be an array
		while ( (node = elem[i++]) ) {
			// Do not traverse comment nodes
			ret += getText( node );
		}
	} else if ( nodeType === 1 || nodeType === 9 || nodeType === 11 ) {
		// Use textContent for elements
		// innerText usage removed for consistency of new lines (jQuery #11153)
		if ( typeof elem.textContent === "string" ) {
			return elem.textContent;
		} else {
			// Traverse its children
			for ( elem = elem.firstChild; elem; elem = elem.nextSibling ) {
				ret += getText( elem );
			}
		}
	} else if ( nodeType === 3 || nodeType === 4 ) {
		return elem.nodeValue;
	}
	// Do not include comment or processing instruction nodes

	return ret;
};

Expr = Sizzle.selectors = {

	// Can be adjusted by the user
	cacheLength: 50,

	createPseudo: markFunction,

	match: matchExpr,

	attrHandle: {},

	find: {},

	relative: {
		">": { dir: "parentNode", first: true },
		" ": { dir: "parentNode" },
		"+": { dir: "previousSibling", first: true },
		"~": { dir: "previousSibling" }
	},

	preFilter: {
		"ATTR": function( match ) {
			match[1] = match[1].replace( runescape, funescape );

			// Move the given value to match[3] whether quoted or unquoted
			match[3] = ( match[3] || match[4] || match[5] || "" ).replace( runescape, funescape );

			if ( match[2] === "~=" ) {
				match[3] = " " + match[3] + " ";
			}

			return match.slice( 0, 4 );
		},

		"CHILD": function( match ) {
			/* matches from matchExpr["CHILD"]
				1 type (only|nth|...)
				2 what (child|of-type)
				3 argument (even|odd|\d*|\d*n([+-]\d+)?|...)
				4 xn-component of xn+y argument ([+-]?\d*n|)
				5 sign of xn-component
				6 x of xn-component
				7 sign of y-component
				8 y of y-component
			*/
			match[1] = match[1].toLowerCase();

			if ( match[1].slice( 0, 3 ) === "nth" ) {
				// nth-* requires argument
				if ( !match[3] ) {
					Sizzle.error( match[0] );
				}

				// numeric x and y parameters for Expr.filter.CHILD
				// remember that false/true cast respectively to 0/1
				match[4] = +( match[4] ? match[5] + (match[6] || 1) : 2 * ( match[3] === "even" || match[3] === "odd" ) );
				match[5] = +( ( match[7] + match[8] ) || match[3] === "odd" );

			// other types prohibit arguments
			} else if ( match[3] ) {
				Sizzle.error( match[0] );
			}

			return match;
		},

		"PSEUDO": function( match ) {
			var excess,
				unquoted = !match[6] && match[2];

			if ( matchExpr["CHILD"].test( match[0] ) ) {
				return null;
			}

			// Accept quoted arguments as-is
			if ( match[3] ) {
				match[2] = match[4] || match[5] || "";

			// Strip excess characters from unquoted arguments
			} else if ( unquoted && rpseudo.test( unquoted ) &&
				// Get excess from tokenize (recursively)
				(excess = tokenize( unquoted, true )) &&
				// advance to the next closing parenthesis
				(excess = unquoted.indexOf( ")", unquoted.length - excess ) - unquoted.length) ) {

				// excess is a negative index
				match[0] = match[0].slice( 0, excess );
				match[2] = unquoted.slice( 0, excess );
			}

			// Return only captures needed by the pseudo filter method (type and argument)
			return match.slice( 0, 3 );
		}
	},

	filter: {

		"TAG": function( nodeNameSelector ) {
			var nodeName = nodeNameSelector.replace( runescape, funescape ).toLowerCase();
			return nodeNameSelector === "*" ?
				function() { return true; } :
				function( elem ) {
					return elem.nodeName && elem.nodeName.toLowerCase() === nodeName;
				};
		},

		"CLASS": function( className ) {
			var pattern = classCache[ className + " " ];

			return pattern ||
				(pattern = new RegExp( "(^|" + whitespace + ")" + className + "(" + whitespace + "|$)" )) &&
				classCache( className, function( elem ) {
					return pattern.test( typeof elem.className === "string" && elem.className || typeof elem.getAttribute !== "undefined" && elem.getAttribute("class") || "" );
				});
		},

		"ATTR": function( name, operator, check ) {
			return function( elem ) {
				var result = Sizzle.attr( elem, name );

				if ( result == null ) {
					return operator === "!=";
				}
				if ( !operator ) {
					return true;
				}

				result += "";

				return operator === "=" ? result === check :
					operator === "!=" ? result !== check :
					operator === "^=" ? check && result.indexOf( check ) === 0 :
					operator === "*=" ? check && result.indexOf( check ) > -1 :
					operator === "$=" ? check && result.slice( -check.length ) === check :
					operator === "~=" ? ( " " + result.replace( rwhitespace, " " ) + " " ).indexOf( check ) > -1 :
					operator === "|=" ? result === check || result.slice( 0, check.length + 1 ) === check + "-" :
					false;
			};
		},

		"CHILD": function( type, what, argument, first, last ) {
			var simple = type.slice( 0, 3 ) !== "nth",
				forward = type.slice( -4 ) !== "last",
				ofType = what === "of-type";

			return first === 1 && last === 0 ?

				// Shortcut for :nth-*(n)
				function( elem ) {
					return !!elem.parentNode;
				} :

				function( elem, context, xml ) {
					var cache, uniqueCache, outerCache, node, nodeIndex, start,
						dir = simple !== forward ? "nextSibling" : "previousSibling",
						parent = elem.parentNode,
						name = ofType && elem.nodeName.toLowerCase(),
						useCache = !xml && !ofType,
						diff = false;

					if ( parent ) {

						// :(first|last|only)-(child|of-type)
						if ( simple ) {
							while ( dir ) {
								node = elem;
								while ( (node = node[ dir ]) ) {
									if ( ofType ?
										node.nodeName.toLowerCase() === name :
										node.nodeType === 1 ) {

										return false;
									}
								}
								// Reverse direction for :only-* (if we haven't yet done so)
								start = dir = type === "only" && !start && "nextSibling";
							}
							return true;
						}

						start = [ forward ? parent.firstChild : parent.lastChild ];

						// non-xml :nth-child(...) stores cache data on `parent`
						if ( forward && useCache ) {

							// Seek `elem` from a previously-cached index

							// ...in a gzip-friendly way
							node = parent;
							outerCache = node[ expando ] || (node[ expando ] = {});

							// Support: IE <9 only
							// Defend against cloned attroperties (jQuery gh-1709)
							uniqueCache = outerCache[ node.uniqueID ] ||
								(outerCache[ node.uniqueID ] = {});

							cache = uniqueCache[ type ] || [];
							nodeIndex = cache[ 0 ] === dirruns && cache[ 1 ];
							diff = nodeIndex && cache[ 2 ];
							node = nodeIndex && parent.childNodes[ nodeIndex ];

							while ( (node = ++nodeIndex && node && node[ dir ] ||

								// Fallback to seeking `elem` from the start
								(diff = nodeIndex = 0) || start.pop()) ) {

								// When found, cache indexes on `parent` and break
								if ( node.nodeType === 1 && ++diff && node === elem ) {
									uniqueCache[ type ] = [ dirruns, nodeIndex, diff ];
									break;
								}
							}

						} else {
							// Use previously-cached element index if available
							if ( useCache ) {
								// ...in a gzip-friendly way
								node = elem;
								outerCache = node[ expando ] || (node[ expando ] = {});

								// Support: IE <9 only
								// Defend against cloned attroperties (jQuery gh-1709)
								uniqueCache = outerCache[ node.uniqueID ] ||
									(outerCache[ node.uniqueID ] = {});

								cache = uniqueCache[ type ] || [];
								nodeIndex = cache[ 0 ] === dirruns && cache[ 1 ];
								diff = nodeIndex;
							}

							// xml :nth-child(...)
							// or :nth-last-child(...) or :nth(-last)?-of-type(...)
							if ( diff === false ) {
								// Use the same loop as above to seek `elem` from the start
								while ( (node = ++nodeIndex && node && node[ dir ] ||
									(diff = nodeIndex = 0) || start.pop()) ) {

									if ( ( ofType ?
										node.nodeName.toLowerCase() === name :
										node.nodeType === 1 ) &&
										++diff ) {

										// Cache the index of each encountered element
										if ( useCache ) {
											outerCache = node[ expando ] || (node[ expando ] = {});

											// Support: IE <9 only
											// Defend against cloned attroperties (jQuery gh-1709)
											uniqueCache = outerCache[ node.uniqueID ] ||
												(outerCache[ node.uniqueID ] = {});

											uniqueCache[ type ] = [ dirruns, diff ];
										}

										if ( node === elem ) {
											break;
										}
									}
								}
							}
						}

						// Incorporate the offset, then check against cycle size
						diff -= last;
						return diff === first || ( diff % first === 0 && diff / first >= 0 );
					}
				};
		},

		"PSEUDO": function( pseudo, argument ) {
			// pseudo-class names are case-insensitive
			// http://www.w3.org/TR/selectors/#pseudo-classes
			// Prioritize by case sensitivity in case custom pseudos are added with uppercase letters
			// Remember that setFilters inherits from pseudos
			var args,
				fn = Expr.pseudos[ pseudo ] || Expr.setFilters[ pseudo.toLowerCase() ] ||
					Sizzle.error( "unsupported pseudo: " + pseudo );

			// The user may use createPseudo to indicate that
			// arguments are needed to create the filter function
			// just as Sizzle does
			if ( fn[ expando ] ) {
				return fn( argument );
			}

			// But maintain support for old signatures
			if ( fn.length > 1 ) {
				args = [ pseudo, pseudo, "", argument ];
				return Expr.setFilters.hasOwnProperty( pseudo.toLowerCase() ) ?
					markFunction(function( seed, matches ) {
						var idx,
							matched = fn( seed, argument ),
							i = matched.length;
						while ( i-- ) {
							idx = indexOf( seed, matched[i] );
							seed[ idx ] = !( matches[ idx ] = matched[i] );
						}
					}) :
					function( elem ) {
						return fn( elem, 0, args );
					};
			}

			return fn;
		}
	},

	pseudos: {
		// Potentially complex pseudos
		"not": markFunction(function( selector ) {
			// Trim the selector passed to compile
			// to avoid treating leading and trailing
			// spaces as combinators
			var input = [],
				results = [],
				matcher = compile( selector.replace( rtrim, "$1" ) );

			return matcher[ expando ] ?
				markFunction(function( seed, matches, context, xml ) {
					var elem,
						unmatched = matcher( seed, null, xml, [] ),
						i = seed.length;

					// Match elements unmatched by `matcher`
					while ( i-- ) {
						if ( (elem = unmatched[i]) ) {
							seed[i] = !(matches[i] = elem);
						}
					}
				}) :
				function( elem, context, xml ) {
					input[0] = elem;
					matcher( input, null, xml, results );
					// Don't keep the element (issue #299)
					input[0] = null;
					return !results.pop();
				};
		}),

		"has": markFunction(function( selector ) {
			return function( elem ) {
				return Sizzle( selector, elem ).length > 0;
			};
		}),

		"contains": markFunction(function( text ) {
			text = text.replace( runescape, funescape );
			return function( elem ) {
				return ( elem.textContent || elem.innerText || getText( elem ) ).indexOf( text ) > -1;
			};
		}),

		// "Whether an element is represented by a :lang() selector
		// is based solely on the element's language value
		// being equal to the identifier C,
		// or beginning with the identifier C immediately followed by "-".
		// The matching of C against the element's language value is performed case-insensitively.
		// The identifier C does not have to be a valid language name."
		// http://www.w3.org/TR/selectors/#lang-pseudo
		"lang": markFunction( function( lang ) {
			// lang value must be a valid identifier
			if ( !ridentifier.test(lang || "") ) {
				Sizzle.error( "unsupported lang: " + lang );
			}
			lang = lang.replace( runescape, funescape ).toLowerCase();
			return function( elem ) {
				var elemLang;
				do {
					if ( (elemLang = documentIsHTML ?
						elem.lang :
						elem.getAttribute("xml:lang") || elem.getAttribute("lang")) ) {

						elemLang = elemLang.toLowerCase();
						return elemLang === lang || elemLang.indexOf( lang + "-" ) === 0;
					}
				} while ( (elem = elem.parentNode) && elem.nodeType === 1 );
				return false;
			};
		}),

		// Miscellaneous
		"target": function( elem ) {
			var hash = window.location && window.location.hash;
			return hash && hash.slice( 1 ) === elem.id;
		},

		"root": function( elem ) {
			return elem === docElem;
		},

		"focus": function( elem ) {
			return elem === document.activeElement && (!document.hasFocus || document.hasFocus()) && !!(elem.type || elem.href || ~elem.tabIndex);
		},

		// Boolean properties
		"enabled": createDisabledPseudo( false ),
		"disabled": createDisabledPseudo( true ),

		"checked": function( elem ) {
			// In CSS3, :checked should return both checked and selected elements
			// http://www.w3.org/TR/2011/REC-css3-selectors-20110929/#checked
			var nodeName = elem.nodeName.toLowerCase();
			return (nodeName === "input" && !!elem.checked) || (nodeName === "option" && !!elem.selected);
		},

		"selected": function( elem ) {
			// Accessing this property makes selected-by-default
			// options in Safari work properly
			if ( elem.parentNode ) {
				elem.parentNode.selectedIndex;
			}

			return elem.selected === true;
		},

		// Contents
		"empty": function( elem ) {
			// http://www.w3.org/TR/selectors/#empty-pseudo
			// :empty is negated by element (1) or content nodes (text: 3; cdata: 4; entity ref: 5),
			//   but not by others (comment: 8; processing instruction: 7; etc.)
			// nodeType < 6 works because attributes (2) do not appear as children
			for ( elem = elem.firstChild; elem; elem = elem.nextSibling ) {
				if ( elem.nodeType < 6 ) {
					return false;
				}
			}
			return true;
		},

		"parent": function( elem ) {
			return !Expr.pseudos["empty"]( elem );
		},

		// Element/input types
		"header": function( elem ) {
			return rheader.test( elem.nodeName );
		},

		"input": function( elem ) {
			return rinputs.test( elem.nodeName );
		},

		"button": function( elem ) {
			var name = elem.nodeName.toLowerCase();
			return name === "input" && elem.type === "button" || name === "button";
		},

		"text": function( elem ) {
			var attr;
			return elem.nodeName.toLowerCase() === "input" &&
				elem.type === "text" &&

				// Support: IE<8
				// New HTML5 attribute values (e.g., "search") appear with elem.type === "text"
				( (attr = elem.getAttribute("type")) == null || attr.toLowerCase() === "text" );
		},

		// Position-in-collection
		"first": createPositionalPseudo(function() {
			return [ 0 ];
		}),

		"last": createPositionalPseudo(function( matchIndexes, length ) {
			return [ length - 1 ];
		}),

		"eq": createPositionalPseudo(function( matchIndexes, length, argument ) {
			return [ argument < 0 ? argument + length : argument ];
		}),

		"even": createPositionalPseudo(function( matchIndexes, length ) {
			var i = 0;
			for ( ; i < length; i += 2 ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		}),

		"odd": createPositionalPseudo(function( matchIndexes, length ) {
			var i = 1;
			for ( ; i < length; i += 2 ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		}),

		"lt": createPositionalPseudo(function( matchIndexes, length, argument ) {
			var i = argument < 0 ? argument + length : argument;
			for ( ; --i >= 0; ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		}),

		"gt": createPositionalPseudo(function( matchIndexes, length, argument ) {
			var i = argument < 0 ? argument + length : argument;
			for ( ; ++i < length; ) {
				matchIndexes.push( i );
			}
			return matchIndexes;
		})
	}
};

Expr.pseudos["nth"] = Expr.pseudos["eq"];

// Add button/input type pseudos
for ( i in { radio: true, checkbox: true, file: true, password: true, image: true } ) {
	Expr.pseudos[ i ] = createInputPseudo( i );
}
for ( i in { submit: true, reset: true } ) {
	Expr.pseudos[ i ] = createButtonPseudo( i );
}

// Easy API for creating new setFilters
function setFilters() {}
setFilters.prototype = Expr.filters = Expr.pseudos;
Expr.setFilters = new setFilters();

tokenize = Sizzle.tokenize = function( selector, parseOnly ) {
	var matched, match, tokens, type,
		soFar, groups, preFilters,
		cached = tokenCache[ selector + " " ];

	if ( cached ) {
		return parseOnly ? 0 : cached.slice( 0 );
	}

	soFar = selector;
	groups = [];
	preFilters = Expr.preFilter;

	while ( soFar ) {

		// Comma and first run
		if ( !matched || (match = rcomma.exec( soFar )) ) {
			if ( match ) {
				// Don't consume trailing commas as valid
				soFar = soFar.slice( match[0].length ) || soFar;
			}
			groups.push( (tokens = []) );
		}

		matched = false;

		// Combinators
		if ( (match = rcombinators.exec( soFar )) ) {
			matched = match.shift();
			tokens.push({
				value: matched,
				// Cast descendant combinators to space
				type: match[0].replace( rtrim, " " )
			});
			soFar = soFar.slice( matched.length );
		}

		// Filters
		for ( type in Expr.filter ) {
			if ( (match = matchExpr[ type ].exec( soFar )) && (!preFilters[ type ] ||
				(match = preFilters[ type ]( match ))) ) {
				matched = match.shift();
				tokens.push({
					value: matched,
					type: type,
					matches: match
				});
				soFar = soFar.slice( matched.length );
			}
		}

		if ( !matched ) {
			break;
		}
	}

	// Return the length of the invalid excess
	// if we're just parsing
	// Otherwise, throw an error or return tokens
	return parseOnly ?
		soFar.length :
		soFar ?
			Sizzle.error( selector ) :
			// Cache the tokens
			tokenCache( selector, groups ).slice( 0 );
};

function toSelector( tokens ) {
	var i = 0,
		len = tokens.length,
		selector = "";
	for ( ; i < len; i++ ) {
		selector += tokens[i].value;
	}
	return selector;
}

function addCombinator( matcher, combinator, base ) {
	var dir = combinator.dir,
		skip = combinator.next,
		key = skip || dir,
		checkNonElements = base && key === "parentNode",
		doneName = done++;

	return combinator.first ?
		// Check against closest ancestor/preceding element
		function( elem, context, xml ) {
			while ( (elem = elem[ dir ]) ) {
				if ( elem.nodeType === 1 || checkNonElements ) {
					return matcher( elem, context, xml );
				}
			}
			return false;
		} :

		// Check against all ancestor/preceding elements
		function( elem, context, xml ) {
			var oldCache, uniqueCache, outerCache,
				newCache = [ dirruns, doneName ];

			// We can't set arbitrary data on XML nodes, so they don't benefit from combinator caching
			if ( xml ) {
				while ( (elem = elem[ dir ]) ) {
					if ( elem.nodeType === 1 || checkNonElements ) {
						if ( matcher( elem, context, xml ) ) {
							return true;
						}
					}
				}
			} else {
				while ( (elem = elem[ dir ]) ) {
					if ( elem.nodeType === 1 || checkNonElements ) {
						outerCache = elem[ expando ] || (elem[ expando ] = {});

						// Support: IE <9 only
						// Defend against cloned attroperties (jQuery gh-1709)
						uniqueCache = outerCache[ elem.uniqueID ] || (outerCache[ elem.uniqueID ] = {});

						if ( skip && skip === elem.nodeName.toLowerCase() ) {
							elem = elem[ dir ] || elem;
						} else if ( (oldCache = uniqueCache[ key ]) &&
							oldCache[ 0 ] === dirruns && oldCache[ 1 ] === doneName ) {

							// Assign to newCache so results back-propagate to previous elements
							return (newCache[ 2 ] = oldCache[ 2 ]);
						} else {
							// Reuse newcache so results back-propagate to previous elements
							uniqueCache[ key ] = newCache;

							// A match means we're done; a fail means we have to keep checking
							if ( (newCache[ 2 ] = matcher( elem, context, xml )) ) {
								return true;
							}
						}
					}
				}
			}
			return false;
		};
}

function elementMatcher( matchers ) {
	return matchers.length > 1 ?
		function( elem, context, xml ) {
			var i = matchers.length;
			while ( i-- ) {
				if ( !matchers[i]( elem, context, xml ) ) {
					return false;
				}
			}
			return true;
		} :
		matchers[0];
}

function multipleContexts( selector, contexts, results ) {
	var i = 0,
		len = contexts.length;
	for ( ; i < len; i++ ) {
		Sizzle( selector, contexts[i], results );
	}
	return results;
}

function condense( unmatched, map, filter, context, xml ) {
	var elem,
		newUnmatched = [],
		i = 0,
		len = unmatched.length,
		mapped = map != null;

	for ( ; i < len; i++ ) {
		if ( (elem = unmatched[i]) ) {
			if ( !filter || filter( elem, context, xml ) ) {
				newUnmatched.push( elem );
				if ( mapped ) {
					map.push( i );
				}
			}
		}
	}

	return newUnmatched;
}

function setMatcher( preFilter, selector, matcher, postFilter, postFinder, postSelector ) {
	if ( postFilter && !postFilter[ expando ] ) {
		postFilter = setMatcher( postFilter );
	}
	if ( postFinder && !postFinder[ expando ] ) {
		postFinder = setMatcher( postFinder, postSelector );
	}
	return markFunction(function( seed, results, context, xml ) {
		var temp, i, elem,
			preMap = [],
			postMap = [],
			preexisting = results.length,

			// Get initial elements from seed or context
			elems = seed || multipleContexts( selector || "*", context.nodeType ? [ context ] : context, [] ),

			// Prefilter to get matcher input, preserving a map for seed-results synchronization
			matcherIn = preFilter && ( seed || !selector ) ?
				condense( elems, preMap, preFilter, context, xml ) :
				elems,

			matcherOut = matcher ?
				// If we have a postFinder, or filtered seed, or non-seed postFilter or preexisting results,
				postFinder || ( seed ? preFilter : preexisting || postFilter ) ?

					// ...intermediate processing is necessary
					[] :

					// ...otherwise use results directly
					results :
				matcherIn;

		// Find primary matches
		if ( matcher ) {
			matcher( matcherIn, matcherOut, context, xml );
		}

		// Apply postFilter
		if ( postFilter ) {
			temp = condense( matcherOut, postMap );
			postFilter( temp, [], context, xml );

			// Un-match failing elements by moving them back to matcherIn
			i = temp.length;
			while ( i-- ) {
				if ( (elem = temp[i]) ) {
					matcherOut[ postMap[i] ] = !(matcherIn[ postMap[i] ] = elem);
				}
			}
		}

		if ( seed ) {
			if ( postFinder || preFilter ) {
				if ( postFinder ) {
					// Get the final matcherOut by condensing this intermediate into postFinder contexts
					temp = [];
					i = matcherOut.length;
					while ( i-- ) {
						if ( (elem = matcherOut[i]) ) {
							// Restore matcherIn since elem is not yet a final match
							temp.push( (matcherIn[i] = elem) );
						}
					}
					postFinder( null, (matcherOut = []), temp, xml );
				}

				// Move matched elements from seed to results to keep them synchronized
				i = matcherOut.length;
				while ( i-- ) {
					if ( (elem = matcherOut[i]) &&
						(temp = postFinder ? indexOf( seed, elem ) : preMap[i]) > -1 ) {

						seed[temp] = !(results[temp] = elem);
					}
				}
			}

		// Add elements to results, through postFinder if defined
		} else {
			matcherOut = condense(
				matcherOut === results ?
					matcherOut.splice( preexisting, matcherOut.length ) :
					matcherOut
			);
			if ( postFinder ) {
				postFinder( null, results, matcherOut, xml );
			} else {
				push.apply( results, matcherOut );
			}
		}
	});
}

function matcherFromTokens( tokens ) {
	var checkContext, matcher, j,
		len = tokens.length,
		leadingRelative = Expr.relative[ tokens[0].type ],
		implicitRelative = leadingRelative || Expr.relative[" "],
		i = leadingRelative ? 1 : 0,

		// The foundational matcher ensures that elements are reachable from top-level context(s)
		matchContext = addCombinator( function( elem ) {
			return elem === checkContext;
		}, implicitRelative, true ),
		matchAnyContext = addCombinator( function( elem ) {
			return indexOf( checkContext, elem ) > -1;
		}, implicitRelative, true ),
		matchers = [ function( elem, context, xml ) {
			var ret = ( !leadingRelative && ( xml || context !== outermostContext ) ) || (
				(checkContext = context).nodeType ?
					matchContext( elem, context, xml ) :
					matchAnyContext( elem, context, xml ) );
			// Avoid hanging onto element (issue #299)
			checkContext = null;
			return ret;
		} ];

	for ( ; i < len; i++ ) {
		if ( (matcher = Expr.relative[ tokens[i].type ]) ) {
			matchers = [ addCombinator(elementMatcher( matchers ), matcher) ];
		} else {
			matcher = Expr.filter[ tokens[i].type ].apply( null, tokens[i].matches );

			// Return special upon seeing a positional matcher
			if ( matcher[ expando ] ) {
				// Find the next relative operator (if any) for proper handling
				j = ++i;
				for ( ; j < len; j++ ) {
					if ( Expr.relative[ tokens[j].type ] ) {
						break;
					}
				}
				return setMatcher(
					i > 1 && elementMatcher( matchers ),
					i > 1 && toSelector(
						// If the preceding token was a descendant combinator, insert an implicit any-element `*`
						tokens.slice( 0, i - 1 ).concat({ value: tokens[ i - 2 ].type === " " ? "*" : "" })
					).replace( rtrim, "$1" ),
					matcher,
					i < j && matcherFromTokens( tokens.slice( i, j ) ),
					j < len && matcherFromTokens( (tokens = tokens.slice( j )) ),
					j < len && toSelector( tokens )
				);
			}
			matchers.push( matcher );
		}
	}

	return elementMatcher( matchers );
}

function matcherFromGroupMatchers( elementMatchers, setMatchers ) {
	var bySet = setMatchers.length > 0,
		byElement = elementMatchers.length > 0,
		superMatcher = function( seed, context, xml, results, outermost ) {
			var elem, j, matcher,
				matchedCount = 0,
				i = "0",
				unmatched = seed && [],
				setMatched = [],
				contextBackup = outermostContext,
				// We must always have either seed elements or outermost context
				elems = seed || byElement && Expr.find["TAG"]( "*", outermost ),
				// Use integer dirruns iff this is the outermost matcher
				dirrunsUnique = (dirruns += contextBackup == null ? 1 : Math.random() || 0.1),
				len = elems.length;

			if ( outermost ) {
				outermostContext = context === document || context || outermost;
			}

			// Add elements passing elementMatchers directly to results
			// Support: IE<9, Safari
			// Tolerate NodeList properties (IE: "length"; Safari: <number>) matching elements by id
			for ( ; i !== len && (elem = elems[i]) != null; i++ ) {
				if ( byElement && elem ) {
					j = 0;
					if ( !context && elem.ownerDocument !== document ) {
						setDocument( elem );
						xml = !documentIsHTML;
					}
					while ( (matcher = elementMatchers[j++]) ) {
						if ( matcher( elem, context || document, xml) ) {
							results.push( elem );
							break;
						}
					}
					if ( outermost ) {
						dirruns = dirrunsUnique;
					}
				}

				// Track unmatched elements for set filters
				if ( bySet ) {
					// They will have gone through all possible matchers
					if ( (elem = !matcher && elem) ) {
						matchedCount--;
					}

					// Lengthen the array for every element, matched or not
					if ( seed ) {
						unmatched.push( elem );
					}
				}
			}

			// `i` is now the count of elements visited above, and adding it to `matchedCount`
			// makes the latter nonnegative.
			matchedCount += i;

			// Apply set filters to unmatched elements
			// NOTE: This can be skipped if there are no unmatched elements (i.e., `matchedCount`
			// equals `i`), unless we didn't visit _any_ elements in the above loop because we have
			// no element matchers and no seed.
			// Incrementing an initially-string "0" `i` allows `i` to remain a string only in that
			// case, which will result in a "00" `matchedCount` that differs from `i` but is also
			// numerically zero.
			if ( bySet && i !== matchedCount ) {
				j = 0;
				while ( (matcher = setMatchers[j++]) ) {
					matcher( unmatched, setMatched, context, xml );
				}

				if ( seed ) {
					// Reintegrate element matches to eliminate the need for sorting
					if ( matchedCount > 0 ) {
						while ( i-- ) {
							if ( !(unmatched[i] || setMatched[i]) ) {
								setMatched[i] = pop.call( results );
							}
						}
					}

					// Discard index placeholder values to get only actual matches
					setMatched = condense( setMatched );
				}

				// Add matches to results
				push.apply( results, setMatched );

				// Seedless set matches succeeding multiple successful matchers stipulate sorting
				if ( outermost && !seed && setMatched.length > 0 &&
					( matchedCount + setMatchers.length ) > 1 ) {

					Sizzle.uniqueSort( results );
				}
			}

			// Override manipulation of globals by nested matchers
			if ( outermost ) {
				dirruns = dirrunsUnique;
				outermostContext = contextBackup;
			}

			return unmatched;
		};

	return bySet ?
		markFunction( superMatcher ) :
		superMatcher;
}

compile = Sizzle.compile = function( selector, match /* Internal Use Only */ ) {
	var i,
		setMatchers = [],
		elementMatchers = [],
		cached = compilerCache[ selector + " " ];

	if ( !cached ) {
		// Generate a function of recursive functions that can be used to check each element
		if ( !match ) {
			match = tokenize( selector );
		}
		i = match.length;
		while ( i-- ) {
			cached = matcherFromTokens( match[i] );
			if ( cached[ expando ] ) {
				setMatchers.push( cached );
			} else {
				elementMatchers.push( cached );
			}
		}

		// Cache the compiled function
		cached = compilerCache( selector, matcherFromGroupMatchers( elementMatchers, setMatchers ) );

		// Save selector and tokenization
		cached.selector = selector;
	}
	return cached;
};

/**
 * A low-level selection function that works with Sizzle's compiled
 *  selector functions
 * @param {String|Function} selector A selector or a pre-compiled
 *  selector function built with Sizzle.compile
 * @param {Element} context
 * @param {Array} [results]
 * @param {Array} [seed] A set of elements to match against
 */
select = Sizzle.select = function( selector, context, results, seed ) {
	var i, tokens, token, type, find,
		compiled = typeof selector === "function" && selector,
		match = !seed && tokenize( (selector = compiled.selector || selector) );

	results = results || [];

	// Try to minimize operations if there is only one selector in the list and no seed
	// (the latter of which guarantees us context)
	if ( match.length === 1 ) {

		// Reduce context if the leading compound selector is an ID
		tokens = match[0] = match[0].slice( 0 );
		if ( tokens.length > 2 && (token = tokens[0]).type === "ID" &&
				context.nodeType === 9 && documentIsHTML && Expr.relative[ tokens[1].type ] ) {

			context = ( Expr.find["ID"]( token.matches[0].replace(runescape, funescape), context ) || [] )[0];
			if ( !context ) {
				return results;

			// Precompiled matchers will still verify ancestry, so step up a level
			} else if ( compiled ) {
				context = context.parentNode;
			}

			selector = selector.slice( tokens.shift().value.length );
		}

		// Fetch a seed set for right-to-left matching
		i = matchExpr["needsContext"].test( selector ) ? 0 : tokens.length;
		while ( i-- ) {
			token = tokens[i];

			// Abort if we hit a combinator
			if ( Expr.relative[ (type = token.type) ] ) {
				break;
			}
			if ( (find = Expr.find[ type ]) ) {
				// Search, expanding context for leading sibling combinators
				if ( (seed = find(
					token.matches[0].replace( runescape, funescape ),
					rsibling.test( tokens[0].type ) && testContext( context.parentNode ) || context
				)) ) {

					// If seed is empty or no tokens remain, we can return early
					tokens.splice( i, 1 );
					selector = seed.length && toSelector( tokens );
					if ( !selector ) {
						push.apply( results, seed );
						return results;
					}

					break;
				}
			}
		}
	}

	// Compile and execute a filtering function if one is not provided
	// Provide `match` to avoid retokenization if we modified the selector above
	( compiled || compile( selector, match ) )(
		seed,
		context,
		!documentIsHTML,
		results,
		!context || rsibling.test( selector ) && testContext( context.parentNode ) || context
	);
	return results;
};

// One-time assignments

// Sort stability
support.sortStable = expando.split("").sort( sortOrder ).join("") === expando;

// Support: Chrome 14-35+
// Always assume duplicates if they aren't passed to the comparison function
support.detectDuplicates = !!hasDuplicate;

// Initialize against the default document
setDocument();

// Support: Webkit<537.32 - Safari 6.0.3/Chrome 25 (fixed in Chrome 27)
// Detached nodes confoundingly follow *each other*
support.sortDetached = assert(function( el ) {
	// Should return 1, but returns 4 (following)
	return el.compareDocumentPosition( document.createElement("fieldset") ) & 1;
});

// Support: IE<8
// Prevent attribute/property "interpolation"
// https://msdn.microsoft.com/en-us/library/ms536429%28VS.85%29.aspx
if ( !assert(function( el ) {
	el.innerHTML = "<a href='#'></a>";
	return el.firstChild.getAttribute("href") === "#" ;
}) ) {
	addHandle( "type|href|height|width", function( elem, name, isXML ) {
		if ( !isXML ) {
			return elem.getAttribute( name, name.toLowerCase() === "type" ? 1 : 2 );
		}
	});
}

// Support: IE<9
// Use defaultValue in place of getAttribute("value")
if ( !support.attributes || !assert(function( el ) {
	el.innerHTML = "<input/>";
	el.firstChild.setAttribute( "value", "" );
	return el.firstChild.getAttribute( "value" ) === "";
}) ) {
	addHandle( "value", function( elem, name, isXML ) {
		if ( !isXML && elem.nodeName.toLowerCase() === "input" ) {
			return elem.defaultValue;
		}
	});
}

// Support: IE<9
// Use getAttributeNode to fetch booleans when getAttribute lies
if ( !assert(function( el ) {
	return el.getAttribute("disabled") == null;
}) ) {
	addHandle( booleans, function( elem, name, isXML ) {
		var val;
		if ( !isXML ) {
			return elem[ name ] === true ? name.toLowerCase() :
					(val = elem.getAttributeNode( name )) && val.specified ?
					val.value :
				null;
		}
	});
}

// EXPOSE
var _sizzle = window.Sizzle;

Sizzle.noConflict = function() {
	if ( window.Sizzle === Sizzle ) {
		window.Sizzle = _sizzle;
	}

	return Sizzle;
};

if ( typeof define === "function" && define.amd ) {
	define(function() { return Sizzle; });
// Sizzle requires that there be a global window in Common-JS like environments
} else if ( typeof module !== "undefined" && module.exports ) {
	module.exports = Sizzle;
} else {
	window.Sizzle = Sizzle;
}
// EXPOSE

})( window );

},{}],"duolingo.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Duolingo = void 0;

var _characters = require("./characters.js");

var _Dictionary = _interopRequireDefault(require("./Dictionary"));

var _sizzle = _interopRequireDefault(require("sizzle"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance"); }

function _iterableToArrayLimit(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

var characterLookup = new _Dictionary.default();
var configuration = {};

function settingsChanged() {
  if (configuration["meanings"] == true) {
    document.body.classList.add("show-meanings");
  } else {
    document.body.classList.remove("show-meanings");
  }
}

chrome.storage.sync.get("traditional", function (result) {
  configuration["traditional"] = result.traditional;
  settingsChanged();
});
chrome.storage.sync.get("meanings", function (result) {
  configuration["meanings"] = result.meanings;
  settingsChanged();
});
chrome.storage.onChanged.addListener(function (changes) {
  configuration[Object.keys(changes)[0]] = changes[Object.keys(changes)[0]].newValue;
  settingsChanged();
});

var Duolingo =
/*#__PURE__*/
function () {
  function Duolingo() {
    _classCallCheck(this, Duolingo);
  }

  _createClass(Duolingo, null, [{
    key: "isLearningChinese",
    value: function isLearningChinese() {
      var state = JSON.parse(localStorage.getItem("duo.state"));
      return state.user.courseId.match(/ZH\-CN_EN/i) != null;
    }
  }, {
    key: "insertCharacter",
    value: function insertCharacter(simplified, traditional) {
      var character = traditional;
      setTimeout(function () {
        var singleChalengeElement = document.querySelector('[data-test="challenge-header"] + span > div > div');

        if (singleChalengeElement !== null) {
          if (singleChalengeElement.childElementCount == 2) {
            var meaning = characterLookup.getMeaning(character);

            if (meaning !== undefined) {
              var div = document.createElement("div");
              var explaination = meaning.explaination == undefined ? " " : meaning.explaination;
              var type = meaning.type == undefined ? " " : meaning.type;
              var englishTranslation = meaning.meaning == undefined ? " " : meaning.meaning;
              div.innerHTML = "<div class=\"meaning\"><div class=\"english\">".concat(englishTranslation, "</div><div class=\"type\">").concat(type, "</div><div class=\"explaination\">").concat(explaination, "</div></div>");
              singleChalengeElement.appendChild(div);
            }
          }
        } else {}
      }, 500);

      if (configuration["traditional"] == false) {
        return;
      }

      if (simplified == traditional) {
        return;
      }

      if (simplified == undefined || simplified == "undefined") {
        return;
      }

      var element = (0, _sizzle.default)(":contains(".concat(simplified, ")")).slice(-1)[0];

      if (element) {
        element.innerHTML = element.innerHTML.replace(simplified, traditional);
      }
    }
  }, {
    key: "checkForChineseCharactersOnLoad",
    value: function checkForChineseCharactersOnLoad() {
      if (!Duolingo.isLearningChinese()) {
        return;
      }

      setTimeout(function () {
        _characters.characters.forEach(function (x, y) {
          var _x$split = x.split("|"),
              _x$split2 = _slicedToArray(_x$split, 2),
              simplified = _x$split2[0],
              traditional = _x$split2[1];

          setTimeout(function () {
            var element = (0, _sizzle.default)(":contains('".concat(simplified, "')"));

            if (element.length != 0) {
              Duolingo.insertCharacter(simplified, traditional);
            }
          }, 10);
        });
      }, 1000);
    }
  }]);

  return Duolingo;
}();

exports.Duolingo = Duolingo;
},{"./characters.js":"characters.js","./Dictionary":"Dictionary.js","sizzle":"../../node_modules/sizzle/dist/sizzle.js"}],"mutationObserver.js":[function(require,module,exports) {
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.mutationObserver = void 0;

var _duolingo = require("./duolingo.js");

var _cache = require("./cache.js");

var _Dictionary = _interopRequireDefault(require("./Dictionary"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

var characterLookup = new _Dictionary.default();
var mutationObserver = new MutationObserver(function (mutations) {
  if (!_duolingo.Duolingo.isLearningChinese()) {
    return false;
  }

  var _loop = function _loop(i) {
    var _loop2 = function _loop2(j) {
      var node = mutations[i].addedNodes[j];
      var nodeSting = node.textContent;
      var asciiStringArray = nodeSting.replace(/[^\x00-\x7F]/g, "");

      if (nodeSting.match(/[\u3400-\u9FBF]/)) {
        var difference = nodeSting.split("").filter(function (x) {
          return asciiStringArray.indexOf(x) == -1;
        });

        if (difference.length == 0) {
          return {
            v: {
              v: void 0
            }
          };
        }

        var chineseCharacters = [];
        var compoundChineseCharacters = [];
        var lastCharacterChinese = false;
        var lastCharacter = "";
        difference.forEach(function (character) {
          if (character > "\u3400" && character < "\u9FBF") {
            chineseCharacters.push(character);
            lastCharacterChinese = true;

            if (lastCharacter != "" && lastCharacterChinese == true) {
              compoundChineseCharacters.push(lastCharacter + character);
            }

            lastCharacter = character;
          } else {
            lastCharacterChinese = false;
            lastCharacter = "";
          }
        });
        compoundChineseCharacters.forEach(function (compoundCharacter) {
          var result = characterLookup.getMeaning(compoundCharacter);

          if (result) {
            chineseCharacters.unshift(compoundCharacter);
          }
        });
        chineseCharacters.forEach(function (character) {
          if (localStorage.getItem(character) !== null) {
            if (localStorage.getItem(character) != undefined) {
              _duolingo.Duolingo.insertCharacter(character, localStorage.getItem(character));
            }

            return null;
          }

          var characterEncoded = encodeURI(character);

          var reqListener = function reqListener(data) {
            if (!mutations[i].addedNodes[j].innerHTML) {
              return;
            }

            if (data.currentTarget.responseText) {
              _cache.Cache.setCharacter(character, data.currentTarget.responseText);
            } //  Duolingo.insertCharacter(character, data.currentTarget.responseText)

          };

          if (navigator.onLine) {// let oReq = new XMLHttpRequest();
            // oReq.addEventListener("load", reqListener);
            // oReq.open(
            //   "GET",
            //   "https://lookup.duochinese.space/?character=" + characterEncoded
            // );
            // oReq.send();
          }
        });
      }
    };

    for (var j = 0; j < mutations[i].addedNodes.length; j++) {
      var _ret2 = _loop2(j);

      if (_typeof(_ret2) === "object") return _ret2.v;
    }
  };

  for (var i = 0; i < mutations.length; i++) {
    var _ret = _loop(i);

    if (_typeof(_ret) === "object") return _ret.v;
  }
});
exports.mutationObserver = mutationObserver;
},{"./duolingo.js":"duolingo.js","./cache.js":"cache.js","./Dictionary":"Dictionary.js"}],"main.js":[function(require,module,exports) {
"use strict";

var _cache = require("./cache.js");

var _mutationObserver = require("./mutationObserver.js");

var _characters = require("./characters.js");

var _duolingo = require("./duolingo.js");
},{"./cache.js":"cache.js","./mutationObserver.js":"mutationObserver.js","./characters.js":"characters.js","./duolingo.js":"duolingo.js"}],"../../node_modules/parcel-bundler/src/builtins/hmr-runtime.js":[function(require,module,exports) {
var global = arguments[3];
var OVERLAY_ID = '__parcel__error__overlay__';
var OldModule = module.bundle.Module;

function Module(moduleName) {
  OldModule.call(this, moduleName);
  this.hot = {
    data: module.bundle.hotData,
    _acceptCallbacks: [],
    _disposeCallbacks: [],
    accept: function (fn) {
      this._acceptCallbacks.push(fn || function () {});
    },
    dispose: function (fn) {
      this._disposeCallbacks.push(fn);
    }
  };
  module.bundle.hotData = null;
}

module.bundle.Module = Module;
var parent = module.bundle.parent;

if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== 'undefined') {
  var hostname = "" || location.hostname;
  var protocol = location.protocol === 'https:' ? 'wss' : 'ws';
  var ws = new WebSocket(protocol + '://' + hostname + ':' + "54877" + '/');

  ws.onmessage = function (event) {
    var data = JSON.parse(event.data);

    if (data.type === 'update') {
      console.clear();
      data.assets.forEach(function (asset) {
        hmrApply(global.parcelRequire, asset);
      });
      data.assets.forEach(function (asset) {
        if (!asset.isNew) {
          hmrAccept(global.parcelRequire, asset.id);
        }
      });
    }

    if (data.type === 'reload') {
      ws.close();

      ws.onclose = function () {
        location.reload();
      };
    }

    if (data.type === 'error-resolved') {
      console.log('[parcel] ✨ Error resolved');
      removeErrorOverlay();
    }

    if (data.type === 'error') {
      console.error('[parcel] 🚨  ' + data.error.message + '\n' + data.error.stack);
      removeErrorOverlay();
      var overlay = createErrorOverlay(data);
      document.body.appendChild(overlay);
    }
  };
}

function removeErrorOverlay() {
  var overlay = document.getElementById(OVERLAY_ID);

  if (overlay) {
    overlay.remove();
  }
}

function createErrorOverlay(data) {
  var overlay = document.createElement('div');
  overlay.id = OVERLAY_ID; // html encode message and stack trace

  var message = document.createElement('div');
  var stackTrace = document.createElement('pre');
  message.innerText = data.error.message;
  stackTrace.innerText = data.error.stack;
  overlay.innerHTML = '<div style="background: black; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; opacity: 0.85; font-family: Menlo, Consolas, monospace; z-index: 9999;">' + '<span style="background: red; padding: 2px 4px; border-radius: 2px;">ERROR</span>' + '<span style="top: 2px; margin-left: 5px; position: relative;">🚨</span>' + '<div style="font-size: 18px; font-weight: bold; margin-top: 20px;">' + message.innerHTML + '</div>' + '<pre>' + stackTrace.innerHTML + '</pre>' + '</div>';
  return overlay;
}

function getParents(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return [];
  }

  var parents = [];
  var k, d, dep;

  for (k in modules) {
    for (d in modules[k][1]) {
      dep = modules[k][1][d];

      if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) {
        parents.push(k);
      }
    }
  }

  if (bundle.parent) {
    parents = parents.concat(getParents(bundle.parent, id));
  }

  return parents;
}

function hmrApply(bundle, asset) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (modules[asset.id] || !bundle.parent) {
    var fn = new Function('require', 'module', 'exports', asset.generated.js);
    asset.isNew = !modules[asset.id];
    modules[asset.id] = [fn, asset.deps];
  } else if (bundle.parent) {
    hmrApply(bundle.parent, asset);
  }
}

function hmrAccept(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (!modules[id] && bundle.parent) {
    return hmrAccept(bundle.parent, id);
  }

  var cached = bundle.cache[id];
  bundle.hotData = {};

  if (cached) {
    cached.hot.data = bundle.hotData;
  }

  if (cached && cached.hot && cached.hot._disposeCallbacks.length) {
    cached.hot._disposeCallbacks.forEach(function (cb) {
      cb(bundle.hotData);
    });
  }

  delete bundle.cache[id];
  bundle(id);
  cached = bundle.cache[id];

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    cached.hot._acceptCallbacks.forEach(function (cb) {
      cb();
    });

    return true;
  }

  return getParents(global.parcelRequire, id).some(function (id) {
    return hmrAccept(global.parcelRequire, id);
  });
}
},{}]},{},["../../node_modules/parcel-bundler/src/builtins/hmr-runtime.js","main.js"], null)
//# sourceMappingURL=/main.map