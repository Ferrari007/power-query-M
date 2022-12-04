let

正则 = (list1,bat) =>
		Web.Page(
			Text.Combine({"<script>document.write('",Text.From(list1),"'.match(/",Text.From(bat),"/g))</script>"})
		)[Data]{0}[Children]{0}[Children]{1}[Text]{0}
,
时间 = (list) => 
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
Result = _extensionLibrary
in
  Result
