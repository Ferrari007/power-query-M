{[
正则 = (list1,bat) =>
		Web.Page(
			Text.Combine({"<script>document.write('",Text.From(list1),"'.match(/",Text.From(bat),"/g))</script>"})
		)[Data]{0}[Children]{0}[Children]{1}[Text]{0}
]}{0}