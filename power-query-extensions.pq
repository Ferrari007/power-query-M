let

WanK = [

时间 = (list) => 
//W[时间]()
let
    重命名 = Table.RenameColumns(list,{"经销商名称", "经销商"}),
	类型 = Table.TransformColumnTypes(Table.TransformColumnTypes(重命名,{{"发送SAP日期", type datetime}}),{{"发送SAP日期", type date}, {"下单日期", type date}, {"数量", type number}, {"单价", type number}, {"总价", type number}, {"总重量", type number}, {"总体积", type number}}),
	公司=Table.AddColumn(类型, "公司", each if Text.Contains([经销商],"远通") then "远通" else if Text.Contains([经销商],"方圆") or Text.Contains([经销商],"格劳") or Text.Contains([经销商],"福") then "泰夏" else if Text.Contains([经销商],"美轮") or Text.Contains([经销商],"鼎")then "东森" else [经销商]),
	日期 = Table.AddColumn(公司, "日期", each if DateTime.Date(DateTime.From([发送SAP日期]))=null then [下单日期] else DateTime.Date(DateTime.From([发送SAP日期]))),
	年度 = Table.AddColumn(日期, "年度", each Number.From(Date.Month([日期])=12 and Date.Day([日期])>25)+Date.Year([日期])),
	月份 = Table.RemoveColumns(Table.AddColumn(年度, "月份", each if Number.From(Date.Day([日期])>25)+Date.Month([日期])>12 then 1 else Number.From(Date.Day([日期])>25)+Date.Month([日期])),{"经销商代码", "物料代码", "总体积", "库位", "发送SAP日期"})
in
	月份
,
政策 = (list,add) => 
//W[政策](,表格)
let
    政策表 = Table.Buffer(add), 
	表格查询=Table.AggregateTableColumn(
		Table.AddColumn(list,
				"平台源",
				(x)=>Table.SelectRows(
						政策表,
						each Text.From(x[年度])=Text.From(_[年度])
							 and (Text.From(x[月份])=Text.From(_[月份]) or Text.From(_[月份])="无")
							 and Text.Contains(x[经销商],_[经销商])
							 and Text.Contains(x[物料名称],_[规格])
							 and Text.Contains(x[物料名称],_[花纹])
							 and Text.Contains(x[物料名称],_[品牌])
					)
				),
		"平台源", {{"属性",each Text.Combine(_,"、"), "一步"}, {"排除",each Text.Combine(_,"、"), "排除"}, {"年返",List.Sum, "年返"}, {"金额", List.Sum, "摊销"}, {"点",List.Sum, "追加"}, {"单胎", List.Sum, "单胎奖励"}}),
	排除调整 = Table.ReplaceValue(表格查询,each [一步],each [年返],(x,y,z)=>if z=null then "年返,回款,追加,提货,追加" else if y<>"" then "年返,回款,追加,提货,追加,单胎" else x,{"排除"}),
	回款 = Table.AddColumn(排除调整, "回款", each if Text.Contains([排除],"回款")then null else 3.5),
	提货 = Table.AddColumn(回款, "提货", each 
			if Text.Contains([排除],"提货")
				then null
				else if Text.Contains([物料名称],"WY")or Text.Contains([物料名称],"星")or Text.Contains([物料名称],"家")
					then 1.5
					else null),
	追加调整 = Table.ReplaceValue(提货,each [排除],each 0,(x,y,z)=>if Text.Contains(y,"追加") then null else x,{"追加"}),
	单胎调整 = Table.ReplaceValue(追加调整,each [排除],each 0,(x,y,z)=>if Text.Contains(y,"单胎") then null else x,{"单胎奖励"}),
	年返调整 = Table.ReplaceValue(单胎调整,each [排除],each 0,(x,y,z)=>if Text.Contains(y,"年返") then null else x,{"年返"}),
	底价 = Table.ReplaceValue(
		Table.RemoveColumns(
			Table.AddColumn(年返调整, "底价", each
				Number.Round([单价]*(1-0.01*(List.Sum({[年返],[回款],[提货],[追加],0}))),2)-List.Sum({[单胎奖励],0})),
			{"排除", "一步"}),
		"有限公司","",Replacer.ReplaceText,{"经销商"})
in
    底价
,
文件夹 = "D:\平台数据"
,
表格调整=(Exclude,Annual,OneStop,Add,Amortization)=>
//W[表格调整](排除表,年返表,一步到位,追加表,摊销表)
let
    享受 = Table.AddColumn(Exclude, "享受", each M[正则](Text.BeforeDelimiter([政策], "不"),"(年返|回款|追加|提货|追加|单胎)")),
    全政策 = Table.AddColumn(享受, "自定义", each "年返,回款,追加,提货,追加,单胎"),
    增加排除 = Table.AddColumn(
		Table.AddIndexColumn(全政策,"索引", 0, 1, Int64.Type), 
			"排除", each Table.AddColumn(
							Table.SelectColumns(全政策,{"自定义", "享受"}),
							"排除",
								(x)=>
									let
										a=List.Transform(
											Record.ToList(x),
											each Text.Split(_,",")
										)
									in
										Text.Combine(List.Difference(a{0},a{1}),",")
								){[索引]}[排除]),
    排除规格 = Table.AddColumn(增加排除, "规格", each M[正则]([政策],"(((\d{3}\/\d{2})|(1?\d(\.\d{2})?))R(\d+(\.\d{1})?|)|R(16|20)|(235|245|275|285|295|315)(\/\d{2})?)")),
    排除花纹 = Table.AddColumn(排除规格, "花纹", each M[正则](List.Accumulate({"235","245","275","285","295","315"},[政策],(s,v)=>Text.Replace(s,v,"")),"([A-Za-z]|)\d{3}([A-Za-z]|)?(\+)?")),
    排除规格调整 = Table.ReplaceValue(排除花纹,each [政策], each 0,
        (x,y,z)=>if x="null"
            then
                if Text.Contains(y,"公制")then "/"
                else if Text.Contains(y,"轻卡") then "R16"
                else "R"
            else x,{"规格"}),
    排除查询 = Table.SelectColumns(Table.ReplaceValue(排除规格调整,"null","R",Replacer.ReplaceValue,{"花纹"}),{"经销商", "年度", "月份", "规格", "花纹", "排除"}),
	点 = Table.AddColumn(Add, "点", each Text.Remove(M[正则]([政策],"(\-|\+)?\d+(\.\d+)?%"), {"%"})),
	单胎 = Table.AddColumn(点, "单胎", each Text.Remove(M[正则]([政策],"\d+(\.\d{1,2})?元"), {"元"})),
    追加规格 = Table.AddColumn(单胎, "规格", each M[正则]([政策],"(((\d{3}\/\d{2})|(1?\d(\.\d{2})?))R(\d+(\.\d{1})?|)|R(16|20)|(235|245|275|285|295|315)(\/\d{2})?)")),
    追加规格调整 = Table.ReplaceValue(追加规格,each [政策], each 0,(x,y,z)=>if Text.Contains(y,"公制")and x="null" then "/" else if x="null" then "R" else x,{"规格"}),
    追加品牌查询=Table.ReplaceValue(Table.ExpandTableColumn(Table.AddColumn(追加规格调整,
            "品牌数据",
            (x)=>Table.SelectRows(
                    Table.FromRows({{"万世达","WYNSTAR"},{"路星","路星"},{"万家达","万家达"},{"超级舰队","超级舰队"},{"飞跃","飞跃"},{"公爵","公爵"},{"第三组","万家达\公爵"},{"千系","WY\路星\万家达"},{"广系","舰队\飞跃\公爵"}},{"关键字", "品牌"}),
                    each Text.Contains(x[政策],_[关键字])
                )
        ), "品牌数据", {"品牌"}, {"品牌"}),null,"R",Replacer.ReplaceValue,{"品牌"}),
    追加查询 = Table.ReplaceValue(
				Table.AddColumn(追加品牌查询, "花纹", each M[正则](List.Accumulate({"235","245","275","285","295","315"},[政策],(s,v)=>Text.Replace(s,v,"")),"([A-Za-z]|)\d{3}([A-Za-z]|)?(\+)?")),
				"null","R",Replacer.ReplaceValue,{"花纹"}),
	源= Table.Combine({OneStop, 排除查询,Annual,Amortization,追加查询}),
    更改的类型 = Table.TransformColumnTypes(源,{{"经销商", type text}, {"年度", type text}, {"月份", type text}, {"规格", type text}, {"花纹", type text}, {"品牌", type text}, {"点", type number}, {"单胎", type number}}),
    替换的值 = Table.ReplaceValue(Table.ReplaceValue(更改的类型,null,"无",Replacer.ReplaceValue,{"年度", "月份", "排除"}),null,"R",Replacer.ReplaceValue,{"品牌", "规格", "花纹"}),
    拆分列 = Table.TransformColumns(替换的值, {{"经销商", each Text.SplitAny(_,"、,\")},{"年度", each Text.SplitAny(_,"、,\")},{"月份", each Text.SplitAny(_,"、,\")},{"品牌", each Text.SplitAny(_,"、,\")},{"规格", each Text.SplitAny(_,"、,\")},{"花纹", each Text.SplitAny(_,"、,\")}}),
    展开 = Table.ExpandListColumn(Table.ExpandListColumn(Table.ExpandListColumn(Table.ExpandListColumn(Table.ExpandListColumn(Table.ExpandListColumn(拆分列, "经销商"), "年度"), "月份"), "品牌"), "规格"), "花纹"),
    筛选数据 = Table.SelectRows(展开, each not((Text.Contains([经销商],"方圆")
				and (Text.Contains([品牌],"WY")or Text.Contains([品牌],"舰")))
			or(Text.Contains([经销商],"美轮")
				and (Text.Contains([品牌],"星")or Text.Contains([品牌],"飞")
					or Text.Contains([品牌],"家")or Text.Contains([品牌],"爵")))
			or(Text.Contains([经销商],"零")
				and (Text.Contains([品牌],"星")or Text.Contains([品牌],"飞"))))
		and(条件{0}[功能]="全数据"
			or(条件{0}[功能]="年数据" and Text.From([年度])=Text.From(条件{0}[年度]))
			or(条件{0}[功能]="月数据" and Text.From([年度])=Text.From(条件{0}[年度])
				and (Text.From([月份])=Text.From(条件{0}[月份])or Text.From([月份])="无")))),
    删除的其他列 = Table.SelectColumns(筛选数据,{"经销商", "年度", "月份", "品牌", "规格", "花纹", "属性", "排除", "年返", "类别", "金额", "点", "单胎"})
in
    删除的其他列
,
发货=(list1,list2)=>
//W[发货](源,平台源)
let
    筛选的行 = Table.SelectRows(list1, each 
		  (Number.From(Date.Month([通知日期])=12 and Date.Day([通知日期])>25)+Date.Year([通知日期])
		  =Number.From(Date.Month(DateTime.FixedLocalNow())=12 and Date.Day(DateTime.FixedLocalNow())>25)+Date.Year(DateTime.FixedLocalNow())
		  and
		  (if Number.From(Date.Day([通知日期])>25)+Date.Month([通知日期])>12 then 1 else Number.From(Date.Day([通知日期])>25)+Date.Month([通知日期]))=
		  (if Number.From(Date.Day(DateTime.FixedLocalNow())>25)+Date.Month(DateTime.FixedLocalNow())>12 then 1 else Number.From(Date.Day(DateTime.FixedLocalNow())>25)+Date.Month(DateTime.FixedLocalNow())))),
    平台 = Table.Combine(List.Transform(list2,each
		Table.ReplaceValue(
			Table.SelectRows(
				Table.ExpandTableColumn(
					Table.AddColumn(_,
					"数据",
					(x)=>Table.SelectRows(
							筛选的行,
							each Text.Contains(_[订单号],x[订单号])
						)),
				"数据", {"通知日期", "序号", "实际日期", "备注"}, {"通知日期", "序号", "实际日期", "备注"}),
			each ([通知日期]<>null and [年度]=条件{0}[年度] and [月份]=条件{0}[月份])),
		each [订单号], each 0,(x,y,z)=>if y<>null then Text.Combine({Text.From(y),"(",Text.From(x),")"}) else null,{"发票号"})
	)),
    总金额 = Table.SelectColumns(Table.AddColumn(平台, "总金额", each [底价]*[数量]),{"通知日期", "序号", "实际日期", "备注", "经销商", "发票号", "物料名称", "数量", "总重量", "底价", "总金额"}),
    小计 = Table.SelectColumns(Table.Group(总金额, {"通知日期", "序号", "实际日期"}, {"表", each
                Table.InsertRows(
                    _,Table.RowCount(_),
                        {
                            [
                                通知日期=null,
								序号=null,
								实际日期=null,
								订单号=null,
								备注=null,
								订单号查询=null,
								平台源=null,
								经销商=null,
                                发票号=[备注]{0},
								物料名称="〖小计〗：",
                                数量=List.Sum([数量]),
								总重量=List.Sum([总重量]),
								底价=null,
								总金额=List.Sum([总金额])
                            ]
                        }
                                )
                                       }
                     ),{"表"}),
    展开分组 = Table.ExpandTableColumn(小计, "表", {"通知日期", "序号", "实际日期", "备注", "经销商", "发票号", "物料名称", "数量", "总重量", "底价", "总金额"}, {"通知日期", "序号", "实际日期", "备注","经销商", "发票号", "物料名称", "数量", "总重量", "底价", "总金额"}),
    总计 = Table.TransformColumnTypes(Table.InsertRows(展开分组
                    ,Table.RowCount(展开分组),
                        {
                            [
                                通知日期=null,
								序号=null,
								实际日期=null,
								订单号=null,
								备注=null,
								订单号查询=null,
								平台源=null,
								经销商=null,
                                发票号=null,
								物料名称="【总计】：",
                                数量=List.Sum(展开分组[数量])/2,
								总重量=List.Sum(展开分组[总重量])/2,
								底价=null,
								总金额=List.Sum(展开分组[总金额])/2
                            ]
                        }
                                ),
    	{{"通知日期", type date}, {"实际日期", type date}}),
    公斤价 =Table.AddColumn(总计,"公斤价",each Number.Round([总金额]/[总重量]/1.13,2)),
    删除的其他列 = Table.SelectColumns(公斤价,{"通知日期", "序号", "实际日期", "经销商", "物料名称", "数量", "底价", "总金额", "总重量", "公斤价", "发票号"})
in
    删除的其他列
,
占比=(list1,list2)=>
//W[占比](数据,平台)
let
    确认订单 = Table.SelectColumns(Table.SelectRows(list1, each [年度]=条件{0}[年度] and [月份]=条件{0}[月份]),{"经销商","物料名称","数量"}),
	标准订单 = Table.ReplaceValue(Table.ReplaceValue(Table.SelectColumns(Table.RenameColumns(list2,{{"产品选择", "物料名称"}}),{"经销商","物料名称","数量"}),"有限公司","",Replacer.ReplaceText,{"经销商"}),"	","",Replacer.ReplaceText,{"经销商"}),
	合并平台 = Table.Combine({确认订单,标准订单}),
    总数 = Table.Group(合并平台,"经销商",{"总数量", each List.Sum([数量]), type number}),
	流量 = Table.Group(Table.SelectRows(合并平台, each
	Text.Contains([物料名称],"12R")and
	    (Text.Contains([物料名称],"310")or
		 Text.Contains([物料名称],"229")or
		 Text.Contains([物料名称],"329")or
		 Text.Contains([物料名称],"330")or
		 Text.Contains([物料名称],"333")or
		 Text.Contains([物料名称],"331")or
		 Text.Contains([物料名称],"680")or
		 Text.Contains([物料名称],"665")or
		 Text.Contains([物料名称],"668")))
	,"经销商",{"数量", each List.Sum([数量]), type number}),
    合并流量 = Table.Sort(Table.ExpandTableColumn(Table.NestedJoin(总数, {"经销商"}, 流量, {"经销商"}, "总数", JoinKind.LeftOuter), "总数", {"数量"}, {"流量"}),{{"经销商", Order.Ascending}}),
	合计 = Table.InsertRows(合并流量,Table.RowCount(合并流量),{[经销商="【合计】：",总数量=List.Sum(合并流量[总数量]),流量=List.Sum(合并流量[流量])]}),
    占比 = Table.AddColumn(合计, "占比", each if [流量]=null then null else Number.ToText([流量]/[总数量],"P1"))
in
    占比
],
Result = WanK
in
  Result
