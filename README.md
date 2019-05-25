<H1>Project：Fund-Investment-Strategy </H1>

以財務模型為基礎，開發五組策略且最好的策略績效高於benchmark 90%。

<H2>Project Flow Chart</H2>

*   策略流程圖
![image](https://github.com/Martin8202/Fund-Investment-Strategy/blob/master/%E7%A8%8B%E5%BC%8F%E6%B5%81%E7%A8%8B%E5%9C%96.jpg)

<H2>Introduction</H2>

*   投資標的：境內資產池 + 境外資產池<br>
*   回測期間：2004年12月31日 ~ 2018年10月26日<br>
*   再平衡時間點：每季的最後一天(3月、6月、9月、12月)<br>
*   手續費：0.3%<br>
*   策略：<br>
    *   等比權重 (Equal Weight)--將標的給予相等權重的投資組合。<br>
    *   波動加權 (Volatility Weighted)--依標的之歷史波動度大小配置權重的投資組合。<br>
    *   動能均權 (Momentum)--挑選具有較強動能的基金，以等比權重的方式組成的投資組合。<br>
    *   混合型 (Combo)--為動能均權策略及波動加權策略之結合。<br>
    *   適應性資產配置 (AAA)--利用動能挑選標的，以能極小化投資組合風險之權重組成的投資組合。<br>
*   benchmark：以整段回測期間報酬為前 25% 的基金透過 equal weighting 所建立的投資組合。<br>

<H2>File Description(由於牽扯專案內容，僅放上模型與視覺化code)</H2>

* D06S03_Strategy_T：<br>
    1.五種策略之程式碼(R)
* D07S04_PlotData_T：<br>
    1.視覺化圖表之程式碼(R)
* shiny：<br>
    1.以shiny呈現之程式檔(R)
    
<H2>Some Example</H2>

*   策略使用之視覺化績效成果，請點選[這裡](https://github.com/Martin8202/Fund-Investment-Strategy/blob/master/%E7%B8%BE%E6%95%88%E7%B5%90%E6%9E%9C.pdf)

*   策略使用之視覺化成果，請點選[這裡](https://github.com/Martin8202/Fund-Investment-Strategy/blob/master/%E5%9C%96%E8%A1%A8%E6%95%B8%E6%93%9A%E5%B0%8D%E7%85%A7.pdf)


 
