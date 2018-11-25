library(Quandl)
library(quantmod)
library(gridExtra)
library(ggplot2)
library(lubridate)
Sys.setlocale("LC_TIME", "C")
Quandl.api_key("Ykn-RwEHL2VBzptNdD6x")

browseURL("https://www.nordnet.fi/webtrader/?locale=fi")

browseURL("https://stockcharts.com/freecharts/candleglance.html?[US]")

#The highly misleading saying that "the stock market is not the economy" 
#is true on a day to day or even month to month basis, but over time these two move together. 
#When they diverge, it is normally a function of emotion, 
#whether measured in valuation premiums/discounts or sentiment extremes.

browseURL("https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top")

getSymbols("A939RX0Q048SBEA", src = "FRED")
fed_gdp <- data.frame(A939RX0Q048SBEA)
fed_gdp$Date <- as.Date(row.names(fed_gdp))

ggplot(fed_gdp, aes(y=A939RX0Q048SBEA, x=Date, color = "cyan"))+
  geom_line()+
  geom_smooth(method='lm', colour = "black", size = 0.1)+
  ylab("")+
  xlab("")+
  ggtitle(paste("Real GDP per Capita", fed_gdp$Date[nrow(fed_gdp)]))+
  scale_x_date(date_breaks = "9 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

#Most importantly, major swings around the trend are due to expansions and contractions in credit - i.e., credit cycles, 
#most importantly 1) a long-term (typically 50 to 75 years) debt cycle and 2) a shorter-term (typically 5 to 8 years) debt cycle
#(i.e., the "business/market cycle").


#These are the interest rate changes that caused all of the recessions and expansions over the last 90 years. - Ray Dalio
#Specifically, note that from the September 1932 low (at 0%) until the May 1981 high (at 19%),
#every cyclical low in interest rates was above the prior cyclical low and every cyclical high was above the prior cyclical high
# - i.e., all of the cyclical increases and decreases were around that 50-year uptrend. (see code below)
#And note that from the May 1981 high in the Fed funds rate (at 19%), 
#until the March 2009 low in the Fed funds rate (0%), every cyclical low in the Fed funds rate was lower
#than the prior low and every cyclical high in interest rates was below the prior cyclical high - i.e., all of the cyclical
#increases and all of the cyclical decreases were around a 27-year downtrend. Each cyclical decline in interest
#rates incrementally reduced debt service payments, lowered the de-facto purchase prices of items bought on
#credit to make them more affordable and boosted the value of assets a notch (having a positive wealth effect).
#So, debt continued to rise relative to income and money, though the trend in debt service payments was
#essentially flat, until interest rates hit 0% and this could not longer continue, at which time the government had
#to print and spend a lot of money to make up for the reduced private sector credit creation and spending.

#When credit is easy and cheap, borrowing and spending will occur; and when it is scarce and expensive,
#borrowing and spending will be less. In the short-term debt cycle, the central bank will control the supply of
#money and influence the amount of credit that the private sector creates by influencing the cost of credit (i.e.,
#interest rates). Changes in private sector credit drive the cycle. Over the long term, typically decades, debt
#burdens rise. This obviously cannot continue forever. When it can't continue a deleveraging occurs.

browseURL("http://www.macrotrends.net/1349/a-debt-fueled-stock-market")
getSymbols(c("FYGFGDQ188S","GFDEGDQ188S"), src = "FRED", from = "1995-01-01")
fed_debt <- data.frame(merge(FYGFGDQ188S, GFDEGDQ188S))
fed_debt$Date <- as.Date(row.names(fed_debt))

ggplot(fed_debt, aes(y=FYGFGDQ188S, x=Date, color = "cyan"))+
  geom_line()+
  geom_line(aes(y=GFDEGDQ188S, x=Date, color = "red"))+
  ylab("")+
  xlab("")+
  ggtitle(paste("Public Debts as Percent of Gross Domestic Product", fed_debt$Date[nrow(fed_debt)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

browseURL("http://www.macrotrends.net/1381/debt-to-gdp-ratio-historical-chart")

#Federal Debt Held by the Public as Percent of Gross Domestic Product
#Total Public Debt as Percent of Gross Domestic Product
#http://www.macrotrends.net/1381/debt-to-gdp-ratio-historical-chart
#We started this cycle at about 150% Total debt to GDP..
#The latest long-term cycle started in 1980 and is coming to an end. Are we deleveraging?
#Upswings in the cycle occur, and are self-reinforcing, in a process by which money growth creates greater debt growth,
#which finances spending growth and asset purchases. Spending growth and higher asset prices allow
#even more debt growth. This is because lenders determine how much they can lend on the basis of the
#borrowers' 1) income/cash flows to service the debt and 2) net worth/collateral, as well as their own capacities
#to lend, and these rise in a self-reinforcing manner.
#Eventually the debt service payments become equal to or larger than the amount we can borrow and the
#spending must decline.


#While the last chart showed the amount of debt relative to GDP, the debt ratio, it is more precise to say that high
#debt service payments (i.e., principal and interest combined), rather than high debt levels, cause debt squeezes
#because cash flows rather than levels of debt create the squeezes that slow the economy. For example, if interest
#rates fall enough, debts can increase without debt service payments rising enough to cause a squeeze. This
#dynamic is best conveyed in the chart below.

#A long-term debt cycle, arises from debts rising faster than both incomes and money until this can't continue
#because debt service costs become excessive, typically because interest rates can't be reduced any more.
#The long-term debt cycle top occurs when 1) debt burdens are high and/or 2) monetary policy doesn't
#produce credit growth (Credit grows with low interest rates(yields) See Money velocity). 
#From that point on, debt can't rise relative to incomes, net worth and money supply.
#That is when deleveraging - i.e., bringing down these debt ratios - begins.



#we started this cycle at 4%-10% for household debt service payments
#http://www.macrotrends.net/1381/debt-to-gdp-ratio-historical-chart
#Some people are worried about high levels of U.S. household debt. 
#When looking at aggregate numbers, there are two ways to consider this question. 
#The first is how much it costs to service this debt as a fraction of disposable (after tax) income. 
#This is shown with the blue line. 
#The second is how much debt there is with respect to the same disposable income measure. 
#This is shown with the red line. 
#In the aggregate, both measures have clearly decreased during the past crisis. 
#Note the scale, though: While service payments decreased by almost one-third, the debt ratio decreased by only one-fifth. 
#And whenever interest rates go back up, service payments will increase.

#Typically, though not always, interest rates decline in reaction to the economic and market declines and central
#banks easing, but they can't decline enough because they hit 0%. As a result, the ability of central banks to
#alleviate these debt burdens, to stimulate private credit growth and to cause asset prices to rise via lower interest
#rates is lost. These conditions cause buyers of financial assets to doubt that the value of the money they will get
#from owning this asset will be more than the value of the money they pay for it. Then monetary policy is
#ineffective in rectifying the imbalance.
#In deleveragings, rather than indebtedness increasing (i.e., debt and debt service rising relative to income and money), it decreases.


#In deflationary depressions/deleveragings, monetary policy is typically ineffective in creating credit because interest rates hit 0%
#and can't be lowered further, so other, less effective ways of increasing money are followed. 
#Credit growth is difficult to stimulate because borrowers remain overindebted, making sensible lending impossible.

#In inflationary deleveragings, monetary policy is ineffective in
#creating credit because increased money growth goes into other currencies and inflation-hedge assets because
#investors fear that their lending will be paid back with money of depreciated value.


#So, With a longer-term higher interest cycle coming up and credit expansions will pick up, there will be higher interest rates
#to be payed and thus less growth

#Dalio says risk premia - the return of risky assets such as bonds compared with cash - are at historically low levels.
#This makes it harder for central banks to keep pushing up the prices of these assets with loose monetary policy, 
#such as low interest rates and quantitative easing, because there is less incentive, or yield, 
#to compensate investors for taking the risk on debt. YIELD CURVE
#10Y yields correlate highly with nominal GDP grwoth

#We may now be living through the early stages of a reversal of the declining trend long-term interest rates which
#began in 1981. 10-yr Treasury yields are the major determinant of mortgage rates(the rate of interest charged by a mortgage lender.)
#and both bottomed in late 2012. Strong loan demand may in part be fueled by a "get it now before rates go higher" mentality, 
#but it may also be fueled by increased confidence and prosperity on the part of homebuyers.
#The inflation expectations embedded in Treasury bond prices have been moving higher since the low of 2014. See more on this later(TIPS)
#http://www.multpl.com/10-year-treasury-rate



#The Mechanics:

#Unlike in recessions, when cutting interest rates and creating more money can rectify this imbalance, in
#deleveragings monetary policy is ineffective in creating credit. In other words, in recessions (when monetary
#policy is effective) the imbalance between the amount of money and the need for it to service debt can be
#rectified by cutting interest rates enough to 1) ease debt service burdens, 2) stimulate economic activity because
#monthly debt service payments are high relative to incomes and 3) produce a positive wealth effect. However, in
#deleveragings, this can't happen. In deflationary depressions/deleveragings, monetary policy is typically
#ineffective in creating credit because interest rates hit 0% and can't be lowered further, so other, less effective
#ways of increasing money are followed. Credit growth is difficult to stimulate because borrowers remain overindebted,
#making sensible lending impossible. In inflationary deleveragings, monetary policy is ineffective in
#creating credit because increased money growth goes into other currencies and inflation-hedge assets because
#investors fear that their lending will be paid back with money of depreciated value.
#In order to try to alleviate this fundamental imbalance, governments inevitably a) create initiatives to encourage
#credit creation, b) ease the rules that require debtors to come up with money to service their debts (i.e., create
#forbearance) and, most importantly, c) print and spend money to buy goods, services and financial assets. The
#printing of and buying financial assets by central banks shows up in central banks' balance sheets expanding and
#the increased spending by central governments shows up in budget deficits exploding. This is shown in the
#following three charts.

# 1) Interest rates hit 0%
#In depressions, a large number of debtors have obligations to deliver more money than they have to meet their
#obligations, and monetary policy is ineffective in reducing debt service costs and stimulating credit growth.

#Typically, monetary policy is ineffective in stimulating credit growth either because interest rates can't be
#lowered (because interest rates are near 0%) to the point of favorably influencing the economics of spending and
#capital formation (this produces deflationary deleveragings), or because money growth goes into the purchase of
#inflation-hedge assets, such as TIPS or Gold so their prices will rise, rather than into credit growth, 
#which produces inflationary deleveragings.

# 2) Fed's production and spending of money grew

#Ray Dalio: Differences in how governments behave in recessions and deleveragings are good clues that signal which one is happening. 
#For example, in deleveragings, central banks typically "print" money that they use to buy large quantities of financial assets
#in order to compensate for the decline in private sector credit, while these actions are unheard of in recessions.
#These show up in changes on their balance sheets that don't occur in recessions.


#M2 to nom GDP is the "money printing" that needs to happen in a beautiful deleveraging (line going up)

# 3) and budget deficits exploded.

#Twin Deficits:

#Net Government Saving:
#https://research.stlouisfed.org/fred2/series/TGDEF
#Page 21: much higher rates than currently in effect are already built into the forecasts for the deficit and debt in the future.
#These forecasts say that deficits will increase
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top

#US Federal Deficit as Percentage of GDP:
#http://www.multpl.com/u-s-federal-deficit-percent/
#Countries with debt above 80 percent of GDP and persistent current-account [trade] 
#deficits are vulnerable to a rapid fiscal deterioration
#Berananke: "Neither experience nor economic theory clearly indicates the threshold at which government debt begins to endanger 
#prosperity and economic stability. But given the significant costs and risks associated with a rapidly rising federal debt, 
#our nation should soon put in place a credible plan for reducing deficits to sustainable levels over time."

#You can tell deleveragings by these three things occurring together, which does not happen at other times.
#Typically, though not necessarily, these moves come in progressively larger dosages as initial dosages of these
#sorts fail to rectify the imbalance and reverse the deleveraging process. However, these dosages do typically
#cause temporary periods of relief that are manifest in bear market rallies in financial assets and increased
#economic activity. For example, in the Great Depression there were six big rallies in the stock market (of between 21% and 48%) 
#in a bear market that totaled 89%, with all of these rallies triggered by these sorts of increasingly
#strong dosages of government actions that were intended to reduce the fundamental imbalance.

#Eventually there is enough "printing of money" or debt monetization to negate the deflationary forces of both debt reduction and
#austerity. When a good balance of debt reduction, austerity, and "printing/monetizing" occurs, debt burdens
#can fall relative to incomes with positive economic growth. In the U.S. deleveraging of the 1930s, this occurred
#from 1933 to 1937.

#The central bank's easing of monetary policy and the movement of investor money to safer investments initially
#drives down short-term government interest rates, steepens the yield curve and widens credit and liquidity spreads. 
#TLT:HYG on stockcharts

#This "printing" of money takes the form of central bank purchases of
#government securities and non-government assets such as corporate securities, equities and other assets. In
#other words, the government "prints" money and uses it to negate some of the effects of contracting credit. This
#is reflected in money growing at an extremely fast rate at the same time as credit and real economic activity
#contract. Traditional economists see that as the velocity of money declining, but it's nothing of the sort. If the
#money creation is large enough, it devalues the currency, lowers real interest rates and drives investors from
#financial assets to inflation-hedge assets. This typically happens when investors want to move money outside
#the currency, and short-term government debt is no longer considered a safe investment.

#As mentioned, currency declines are typically acceptable to governments because a weaker currency is
#stimulative for growth and helps to negate deflationary pressures. In depressions we see currency devalued.
#a strong dollar and weak global demand helped to push exports to more than a five-and-a-half-year low, 
#suggesting that trade will continue to weigh on economic growth in the first quarter.
#The trade-weighted dollar is down 3.0% from its recent peak on January 20, but remains up 19% since July 1, 2014. 
#That's clearly weighing on exports. This is yet another reason to postpone further rate hikes so that the dollar doesn't move still higher. 
getSymbols(c("TWEXMPA","TWEXBPA"), src = "FRED", from = "1995-01-01")
DOLLAR <- data.frame(TWEXMPA, TWEXBPA)
DOLLAR$Date <- as.Date(row.names(DOLLAR))

dollar1 <- ggplot(DOLLAR, aes(y=TWEXMPA, x=Date, color = "cyan"))+
  geom_line()+
  geom_line(aes(y=TWEXBPA, x=Date, color = "red"))+
  geom_hline(yintercept = mean(DOLLAR$TWEXMPA), color = "cyan", linetype = "dashed")+
  geom_hline(yintercept = mean(DOLLAR$TWEXBPA), color = "red", linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Real Trade Weighted U.S. Dollar Indices", DOLLAR$Date[nrow(DOLLAR)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
#With over 40% of S&P 500 revenues coming from abroad, a weaker dollar will boost foreign sales, particularly later this year and into 2018.
#Companies in the S&P derive about half of their sales from outside of the US. 
#When the dollar rises in value, the value of sales earned abroad (in foreign currency) falls. 
#If foreign sales grow 5% but the dollar gains 5% against other currencies, then sales growth will be zero in dollar terms.
#A higher dollar corresponds with lower sales (from Yardeni).

#If conditions in the U.S. were booming, not only would you expect to see much higher interest rates, 
#you would also expect to see a very strong dollar. However, as Chart shows, the dollar today is only modestly higher than its 
#long-term average vis a vis other currencies.

#However, we expect this weakness to be temporary and inflation to edge up through 2018, helped by a weaker dollar and tightening labor and housing markets. 
#In addition, inflation calculated by the personal consumption deflator continues to approach the Federal Reserve’s 2% target in 2017, which helped to justify the interest rate hikes in March and June.
dollar2 <- ggplot(DOLLAR, aes(y=Delt(TWEXMPA, k=12)*-1, x=Date, color = "cyan"))+
  geom_line()+
  geom_line(aes(y=Delt(TWEXBPA, k=12)*-1, x=Date, color = "red"))+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY Real Trade Weighted U.S. Dollar Indices (Inverted scale)")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(dollar1, dollar2)
#Importantly, the dollar has since stabilized. In the past year (thru 2Q17), the dollar appreciated by just 3%. 
#For the current quarter (3Q17), the dollar is on track for zero appreciation.

#One reason foreign central bankers favor weaker currencies is because that raises the prices of commodities 
#quoted in local currencies, and helps boost inflation.

#Debtor, current account deficit countries are especially vulnerable to capital withdrawals and currency weakness
#as foreign investors also tend to flee due to both currency weakness and an environment inhospitable to good returns on capital. 
#However, this is less true for countries that have a great amount of debt denominated in their
#own currencies (like the United States in the recent period and in the Great Depression) as these debts create a
#demand for these currencies. Since debt is a promise to deliver money that one doesn't have, this is essentially a
#short squeeze that ends when a) the shorts are fully squeezed (i.e., the debts are defaulted on) and/or b) enough
#money is created to alleviate the squeeze, and/or c) the debt service requirements are reduced in some other way (e.g., forbearance).
#The risk at this stage of the process is that the currency weakness and the increased supply of money will lead to
#short-term credit (even government short-term credit) becoming undesirable, causing the buying of inflationhedge
#assets and capital flight rather than credit creation. For foreign investors, receiving an interest rate that is
#near 0% and having the foreign currency that their deposits are denominated in decline produces a negativereturn; 
#so this set of circumstances makes holding credit, even government short-term credit, undesirable.

#Total Current Account Balance for the United States?, Percent of GDP:
#https://research.stlouisfed.org/fred2/series/BPBLTT01USQ188S
#The current account is an important indicator about an economy's health. 
#It is defined as the sum of the balance of trade (goods and services exports less imports), 
#net income from abroad and net current transfers. A positive current account balance indicates that the nation is a net lender
#to the rest of the world, while a negative current account balance indicates that it is a net borrower from the rest of the world.
#Since the trade balance (exports minus imports) is generally the biggest determinant of the current account surplus or deficit, 
#the current account balance often displays a cyclical trend. During a strong economic expansion, import volumes typically surge; 
#if exports are unable to grow at the same rate, the current account deficit will widen.
#The currency exchange rate exerts a significant influence on the trade balance, and by extension, on the current account. 
#An overvalued currency makes imports cheaper and exports less competitive, thereby widening the current account deficit
#If an economy is running a current account deficit, it is absorbing 
#(absorption = domestic consumption + investment + government spending) more than that it is producing. 

#Reminder: GDP Growth and Debt-to-GDP are negatively correlated! (Debt up relative to GDP = GDP growth down.)
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#page 26
#This chart looks at the Current Account Balance of the U.S. as a percent of GDP and the value of the U.S. Dollar. 
#As this page shows, a stronger U.S. dollar tends to lead a larger current account deficit as a percent of GDP. 



#Depression: During this time nominal interest rates must be kept below nominal growth rates to reduce the debt burdens. 
#If interest rates are at 0% and there is deflation, central banks must "print" enough money to raise nominal growth.

#What next:

#The deleveraging process reduces debt/income ratios. When debt burdens become too large, deleveragings must happen.

#the differences between deleveragings depend on the amounts and paces of 1) debt reduction, 
#2) austerity, 3) transferring wealth from the haves to the have-nots, and 4) debt monetization. Each
#one of these four paths reduces debt/income ratios, but they have different effects on inflation and growth. Debt
#reduction (i.e., defaults and restructurings) and austerity are both deflationary and depressing while debt
#monetization is inflationary and stimulative. Ugly deleveragings get these out of balance while beautiful ones
#properly balance them. In other words, the key is in getting the mix right..

#Typically, in response to a debt crisis the going to these four steps takes place in the following order:

#At first, problems servicing debt and the associated fall off in debt growth cause an economic
#contraction in which the debt/income ratios rise at the same time as economic activity and
#financial asset prices fall. I will call this phase an "ugly deflationary deleveraging". Debt
#reduction (i.e., defaults and restructurings) and austerity without material debt monetization
#characterize this phase. During this period, the fall in private sector credit growth (MZMV) and the tightness
#of liquidity(high interest rates) lead to declines in demand for goods, services and financial assets. The financial bubble
#bursts when there is not enough money to service the debt and debt defaults and restructurings hit
#people, especially leveraged lenders (banks), like an avalanche that causes fears. These justified
#fears feed on themselves and lead to a liquidity crisis. As a result, policy makers find themselves in a
#mad scramble to contain the defaults before they spin out of control.

#In the second phase of the typical deleveraging the debt/income ratios decline at the same
#time as economic activity and financial asset prices improve. This happens because there is
#enough "printing of money/debt monetization" to bring the nominal growth rate above the
#nominal interest rate and a currency devaluation to offset the deflationary forces. This creates a "beautiful deleveraging".
#The best way of negating the deflationary depression is for the central
#bank to provide adequate liquidity and credit support and, depending on different key entities' need
#for capital, for the central government to provide that too. This takes the form of the central bank
#both lending against a wider range of collateral (both lower quality and longer maturity) and buying
#(monetizing) lower-quality and/or longer-term debt. This produces relief and, if done in the right
#amounts, allows a deleveraging to occur with positive growth. The right amounts are those that a)
#neutralize what would otherwise be a deflationary credit market collapse and b) get the nominal
#growth rate marginally above the nominal interest rate to tolerably spread out the deleveraging
#process. At such times of reflation, there is typically currency weakness, especially against gold, 
#but this will not produce unacceptably high inflation because the reflation is simply negating the deflation.


#History has shown that those who have done it quickly and well (like the US in 2008/9)
#have derived much better results than those who did it late (like the US in 1930-33). However, there
#is such a thing as abusive use of stimulants. Because stimulants work so well relative to the
#alternatives, there is a real risk that they can be abused, causing an "ugly inflationary deleveraging".

#When there is too much "printing of money/monetization" and too severe a currency
#devaluation (which are reflationary) relative to the amounts of the other three alternatives
#"ugly inflationary deleveragings" can occur.
#they can occur slowly and late in the deleveraging process of reserve currency countries, after a long
#time and a lot of stimulation that is used to reverse a deflationary deleveraging.

#By the way, transfers of wealth from the have to the have-nots typically occur in many forms (e.g.,
#increased taxes on the wealthy, financial support programs such as those the "rich" European countries
#are providing to the overly indebted ones, etc.) throughout the process, but they rarely occur in amounts
#that contribute meaningfully to the deleveraging (unless there are "revolutions").


#Three ways to deleverage and how to spot each:

#1) 
#The Ugly Deflationary Deleveragings (i.e., when the economy was bad while the debt/income ratio rose)
#a) money printing was limited (See fed balancheet, Adjusted monetary base, Money base to gdp), 
#b) nominal growth was below nominal rates, 
#c) the currency was generally strong, and d) the debt/income ratios rose because of the combination of
#interest payment costs and nominal incomes falling or stagnating.

#income and credit collapsed, with nominal growth rates falling
#significantly below nominal interest rates, and the economy contracted while the debt/income ratio rose. As
#shown, it followed the stock market bubble bursting in September 1929. As a result of that private sector
#deleveraging, incomes collapsed, to the point that they were declining by nearly 30% per year at the end of 1932.
#Because of the fall in incomes, debt/GDP rose from roughly 150% to 250% of GDP (as shown on the left).
#Through this time stocks fell by more than 80% (as shown on the right).

#In March of '33, the Fed eased by devaluing the dollar against gold and kept interest rates low for many years.
#Most of the additional balance sheet expansion was to buy gold to keep the value of the dollar depressed. While
#the Fed made money easy through low rates and currency, it did not directly buy many risky assets

#the catalyst for the recovery was the printing and dollar devaluation against gold. Price levels
#turned at this point, from declining at an average rate of 8% to increasing roughly 2% per year. This is a good
#example of how printing negated deflation rather than triggering high inflation.

#Japan has been stuck in a moderate "ugly deflationary deleveraging" for over 20 years. In 1989
#the private sector debt bubble burst and government sector debt/fiscal expansion began, but there was never
#adequate "money printing/monetization" to cause nominal growth to be above nominal interest rates and to have the currency devalue.
#The BOJ has "printed/monetized" very little in duration-adjusted terms throughout the deleveraging process,
#with most of the printing that it has done going to short-term cash-like assets of little duration. As result, it has
#failed to reflate and the government is building a terrible debt burden.

#2) 
#The Beautiful Deleveragings (i.e., when the economy was growing in a balanced way with the debt/income ratio declining)
#money printing (See fed balancheet, Adjusted monetary base) and currency devaluations were sizable, nominal growth
#rates were pushed above nominal interest rates and the debt/income ratios fell(govt and private). During the reflation periods, a
#recovery in nominal incomes lessened the debt/income burdens. Naturally, in cases in which the downturns that
#preceded these periods were very deep (e.g., 1930-32 in the US) the rebounds were greater.

#keep nominal growth above nominal interest rates, which was the most important influence in lowering the debt/income ratio.
#Devaluing currency is also an important boost

#the US has quickly entered a
#reflation and ended the "ugly deflationary deleveraging" phase of the process (which lasted from July 2008, just
#before Lehman fell, to March 2009, when the Fed instituted its aggressive program of quantitative easing to
#monetize the debts). During the "ugly" phase, incomes fell, debt burdens rose from about 340% GDP to 370%
#and stocks lost almost half their value. Because so much debt around the world is dollar denominated, the
#contraction in global credit and dollar liquidity created a squeeze for dollars, and the dollar strengthened
#significantly against a trade-weighted basket. Exports collapsed faster than domestic demand. Following the
#reflation that began in March 2009, incomes recovered, debt burdens fell below their initial starting level to
#around 335% and stocks recovered all of their losses. At this time, the credit markets are largely healed and
#private sector credit growth is improving. Thus far, this deleveraging would win my award of the most beautiful
#deleveraging on record. The key going forward will be for policy makers to maintain balance so that the
#debt/income ratio keeps declining in an orderly way.

#The magnitude of the easing by the Fed has been substantial. Not only did the Fed cut rates and backstop
#essential credit during the liquidity crisis, but it pursued one of the most aggressive easing policies by pushing
#money into risky assets. The Fed began to push money into the system with the announcement of a significant
#QE1 in March 2009 with the purchase of Treasuries and agency-backed bonds. The Fed further increased its
#holdings of longer duration government debt (mostly Treasuries) with QE2 starting in August 2010 and
#Operation Twist starting in the fall 2011. During these three periods, changes in asset holdings on a durationadjusted
#basis (equivalent to 10-year duration) peaked at 8%, 5% and roughly 2% of GDP annualized pace respectively.

#3) 
#The Ugly Inflationary Deleveraging (i.e., when the economy was bad at the same time as there was hyperinflation that wiped out the debts)
#Gold to currency jumps high as do other inflation hedges. Money printing is high.

#In the long run, in otherwords in the larger debt cycle, high interest rates are detrimental to stock returns as you can see from
#the slopes of the regression lines. If we are starting a new long-term debt cycle, we can expect slower growth.

#Summary: Long-term high rates are bad for stock returns. Short-term they are good.

#---

#Explanation of short-term cycles:

#A short-term debt cycle, which is commonly called the business cycle, arises from the rate of growth in
#spending, funded by the rates of growth in money and credit being faster than the rate of growth in the capacity to produce 
#leading to price increases until the rate of growth in spending is curtailed by tight money and credit, 
#at which time a recession occurs. 

#In other words, a recession is an economic contraction that is due to a contraction in private sector debt growth arising from 
#tight central bank policy (usually to fight inflation), which ends when the central bank eases. 
#Recessions end when central banks lower interest rates to stimulate demand for goods and services and the credit growth that
#finances these purchases, because lower interest rates reduce debt service costs; lower monthly payments of items bought on credit,
#which stimulates the demand for them; and raise the prices of income-producing assets like stocks,
#bonds and real estate through the present value effect of discounting their expected cash flows at the lower interest rates,
#producing a "wealth effect" on spending.

#Key words: Rate of Growth funded by rate of growth in credit which is larger than rate of growth in capacity to produce.

#The expansion phase of the cycle:
#The "early-cycle". typically begins with the demand for
#interest rate sensitive items (e.g., housing and cars) and retail sales picking up because of low interest rates
#and lots of available credit. It is also supported by prior inventory liquidations stopping and inventory
#rebuilding starting. This increased demand and rising production pulls the average workweek and then
#employment up. Credit growth is typically fast, economic growth is strong (i.e., in excess of 4%), inflation is
#low, growth in consumption is strong, the rate of inventory accumulation is increasing, the U.S. stock market
#is typically the best investment (because there is fast growth and interest rates aren't rising because inflation
#isn't rising) and inflation-hedge assets and commodities are the worst-performing assets.

#This is typically followed by what I call the "mid-cycle".
#when economic growth slows substantially (i.e., to around 2%), inflation remains low, growth in
#consumption slows, the rate of inventory accumulation declines, interest rates dip, the stock market rate of
#increase tapers off, and the rate of decline in inflation-hedge assets slows.

#This in turn is followed by the "late-cycle". At this point, economic
#growth picks up to a moderate pace (i.e., around 3.5-4%), capacity constraints emerge, but credit and
#demand growth are still strong. So, inflation begins to trend higher, growth in consumption rises, inventories
#typically pick up, interest rates rise, the stock market stages its last advance and inflation-hedge assets
#become the best-performing investments.

#This is typically followed by the tightening phase of the expansion. In this phase, actual or anticipated
#acceleration of inflation prompts the Fed to turn restrictive, which shows up in reduced liquidity, interest
#rates rising and the yield curve flattening or inverting. This, in turn, causes money supply and credit growth
#to fall and the stock market to decline before the economy turns down.

#The recession phase of the cycle follows and occurs in two parts.
#In the early part of the recession, the economy contracts, slack returns (as measured by the GDP gap,
#capacity utilization and the unemployment rate), stocks, commodities and inflation-hedge assets fall and
#inflation declines because the Fed remains tight.

#In the late part of the recession, the central bank eases monetary policy as inflation concerns subside and
#recession concerns grow. So interest rates decline and the lower interest rates cause stock prices to rise
#(even though the economy hasn't yet turned up) while commodity prices and inflation-hedge assets continue
#to be weak. The lower interest rates and higher stock prices set the stage for the expansion part of the cycle
#to begin.

#Given the previously described linkages, inflation doesn't normally heat up until the slack in the economy is largely
#eliminated, and the Fed doesn't normally turn restrictive until inflation rises. An expansion that starts off after a
#deep recession (i.e., one that produces lots of slack) is bound to last longer than an expansion that begins with
#less excess capacity. Similarly, as the cycle progresses through its various stages as a function of the sequences
#just described, the rate at which it progresses will be a function of the forcefulness of the influences that drive its
#progression. For example, an expansion that is accompanied by an aggressively stimulative central bank is likely
#to be stronger and evolve more quickly than one that is accompanied by a less stimulative monetary policy.

getSymbols("GPDIC1", src = "FRED")
ACILACB <- data.frame(GPDIC1)
ACILACB$Date <- as.Date(row.names(ACILACB))
ACILACB$yoy <- Delt(ACILACB$GPDIC1, k=12)
ACILACB <- ACILACB[ACILACB$Date >= "1990-01-01",]

ACILACB_plot <- ggplot(ACILACB, aes(y=GPDIC1, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Real Gross Private Domestic Investment")+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

ACILACB_plot2 <- ggplot(ACILACB, aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Real Gross Private Domestic Investment")+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(ACILACB_plot, ACILACB_plot2)


#What follows is a brief review of the state of money. There's plenty of it out there

browseURL("https://research.stlouisfed.org/useraccount/fredgraph/?graph_id=390067")
#It's true that there is a whole lot of money out there that isn't being spent. 
#But that's because people-and banks-feel a need to accumulate money and deleverage.
#One important measure of money is the amount of money relative to the size of the economy, 
#shown in the chart above. This is a good proxy for the demand for money, 
#and it can be thought of as the amount of one's annual income that is kept in ready cash form
#it has reached an all-time high
#That's unusual, because for many years these accounts have paid little if any interest. 
#That must mean the demand for the safety of these accounts is very strong, 
#since they offer little benefit beyond being a safe repository of money that is easily accessible.
getSymbols("SVGCBNS", src = "FRED", from = "1995-01-01")
bank <- data.frame(SVGCBNS)
bank$Date <- as.Date(row.names(bank))
bank$yoy <-  Delt(bank$SVGCBNS, k=12)
bank <- bank[bank$Date >= "1990-01-01",]

bank_plot <- ggplot(bank, aes(y=log(SVGCBNS), x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Savings Deposits at Commercial Banks (Log)")+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

bank_plot2 <- ggplot(bank, aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Savings Deposits")+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(bank_plot, bank_plot2)
#Recessions and economic turmoil almost always make bank savings deposits attractive, especially relative to riskier alternatives. 
#Demand for safety typically declines in the latter stages of an expansion, when people's appetite for risk rises in line with rising confidence. 
#So it's normal for rising confidence to be reflected in slower growth of bank deposits.
#The chart above tracks the history of bank savings deposits, 
#which have been growing at about an 8% annual rate in recent years. 
#Following the 2008 financial crisis, these accounts grew at double-digit rates for several years. 
#Flight to safety by consumers and banks invested their deposit inflows with the Fed (safety).

#as a result of a series of regulatory requirements that have been implemented since 2008, 
#banks' demand for reserves has skyrocketed. 
#It's possible that half or more of the current excess reserves held by banks today is effectively dictated by various requirements and requirements to come.

#there is plenty of cash left over to be pulled to help finance share buybacks. 
#The Fed only needs to lower the rates on reserves or to bully its 
#shareholders into using more reserves in the economy if there is a need. 

#In other words, banks have accumulated a mountain of excess reserves not because they don't want to lend against those reserves but because they had to accumulate them in order to survive. 
#Banks may therefore not be as flush with excess reserves as the numbers suggest. 
#They have responded to changing regulatory burdens by becoming more conservative and hoarding cash. 
#Consumers have done the same in response to the loss of confidence that still persists in the wake of the 2008 financial crisis. 
#In short, there are lots of reasons why the demand for money is strong.

#Almost all of the increase in reserves is still sitting idle at the Fed, 
#which means that banks are content to hold a huge portion of their balance sheets in reserves paying only 0.25%.
#As is the case with consumers who love savings deposits for their safety rather than their yield, this reflects a 
#banking system that is still very risk averse. Should that change, however, and should banks become more willing to lend,
#they have an almost unlimited potential to do so. 

#The growth of credit and of the money supply (a bit more than 6% per year) 
#has been well in excess of the growth of nominal GDP. 
#From these facts, it would be hard to make the case that our problems today stem from any lack of money or lack of lending.

#Demand for Cash: https://fredblog.stlouisfed.org/2015/01/the-velocity-of-money/
#Down: This can be explained partly by a flight to the safety of insured bank deposits during the financial crisis.

#The Federal Reserve has chosen to define "money" in terms of aggregates (i.e., currency plus various forms of credit - M1, M2, etc.), 
#but this is misleading. Virtually all of what they call money is credit
#Fed controls interest rates and has lowered them in this cycle. It should increase them in the next making borrowing more
#expensive.

#MONEY DEMAND:
#the ratio of M2 to nominal GDP. 
#Nominal GDP is a proxy for national income, and M2 is arguably the best measure of readily-spendable money. 
#Think of this chart as measure of how much money the average person wants to hold relative to his annual income. 
#It's risen from 50% prior to the Great Recession to 70% today. 
#That's huge, and one of the defining characteristics of the current business cycle expansion—a massive increase in the demand for money.
#People now have parked an unprecedented amount of their annual cash flow in retail money market funds, 
#small time deposits, currency and bank savings deposits—and the bulk of the increased money holdings are in the form of bank savings deposits. 
#What's even more impressive, however, is that none of these vehicles pays much in the way of interest. 
#People are holding lots of money not because they like the return on money, but because of its safety.

#I note the recent rise in gold prices (+15% year to date) and the decline in the value of the dollar (-9% year to date). 
#Both are symptomatic of an excess of money relative to the demand for it, and thus they could be evidence that the Fed is falling behind the inflation curve.

#Much hangs on just how much and how fast the public's demand for money declines, and how fast and how much the Fed is able to offset that by raising interest rates and reducing excess reserves in the banking system. 
#So far things look OK, but it certainly pays to keep an eye on these developments.

#Most of what economists call the velocity of money is not the velocity of money at all. It is credit creation. - Ray Dalio
getSymbols(c("M1V","M2V","MZMV"), src = "FRED")
credit <- data.frame(merge(M1V,M2V,MZMV))
credit$Date <- as.Date(row.names(credit))

credit_plot <- ggplot(credit, aes(y=M1V, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Credit Creation (M1V)")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

credit_plot2 <- ggplot(credit, aes(y=M2V, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Credit Creation (M2V)")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

credit_plot3 <- ggplot(credit, aes(y=MZMV, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Credit Creation (MZMV)")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(credit_plot, credit_plot2, credit_plot3)

#the rate at which people spend money (create credit)
#MZMV is the broadest measure of Credit Creation or the rate at which people spend money (create credit(transactions))
#MZM better represents money readily available within the economy for spending and consumption.
#Credit creation tends to turn down before fed starts to lower interest rates.
#recessions and depressions develop from declines in demand, typically due to a fall-off in credit creation.

#The velocity of money is a measure of how fast Credit Creation is in the economy.
#Credit Creation is related to inflation, as the faster Credit is Created, the more pressure
#exists on prices, and as a leading indicator of long-term interest rates, again because it reflects
#inflationary pressures. When Credit Creation rises, the stock markets tend to rise slower then when Credit Creation goes down.
#Inflationary pressures from increased Credit Creation put a damper on stock market prices.

#If the Credit Creation is increasing, then more transactions are occurring between individuals in an economy. 
#M1 is the Credit supply of currency in circulation (notes and coins, traveler's checks, demand deposits, and checkable deposits). 
#A decreasing Credit Creation of M1 might indicate fewer short- term consumption transactions are taking place.
#The broader M2 component includes M1 in addition to saving deposits, certificates of deposit (less than $100,000), 
#and money market deposits for individuals. 
#The Credit Creation of MZM helps determine how often financial assets are switching hands within the economy.

#prosperity equals high demand, and in our creditbased economy, strong demand equals strong real credit growth.
#changes in demand precede changes in capacity

#Comparing the Credit Creation of M1 and M2 provides some insight into how quickly the economy is spending and how quickly it is saving.
ggplot(credit, aes(y=M1V/M2V, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Spending (M1V) vs Saving (M2V)")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
#Up is spending is increasing, which is faster economic growth
#Demand for money: 
browseURL("https://fredblog.stlouisfed.org/2015/01/the-velocity-of-money/")


#Some people are worried about high levels of U.S. household debt. 
#When looking at aggregate numbers, there are two ways to consider this question. 
#The first is how much it costs to service this debt as a fraction of disposable (after tax) income. 
#This is shown with the blue line. 
#The second is how much debt there is with respect to the same disposable income measure. 
#This is shown with the red line. 
#In the aggregate, both measures have clearly decreased during the past crisis. 
#Note the scale, though: While service payments decreased by almost one-third, the debt ratio decreased by only one-fifth. 
#And whenever interest rates go back up, service payments will increase.

#Households have deleveraged. This is a good thing as they can start to accumulate more debt, but at a slower rate as
#interest rates rise.
#Since the start of the latest debt cycle, household debt seems to have topped. Is this the start of long-term deleveraging?
#More: https://fredblog.stlouisfed.org/2015/01/on-household-debt/
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#page 17 highlighting the consumer deleveraging we have observed over the past few years.

#While these debt problems can occur for many reasons, most classically they occur because
#investment assets are bought at high prices and with leverage - i.e., because debt levels are set on the basis of
#overly optimistic assumptions about future cash flows. As a result of this, actual cash flows fall short of what's
#required for debtors to service their debts. Ironically, quite often in the early stages the cash flows fall short
#because of tight monetary policies that are overdue attempts to curtail these bubble activities
#In fact, while debtfinanced financial booms that are accompanied by low inflation are typically precursors of busts, 
#at the time they typically appear to be investment-generated productivity booms

All <- Quandl("USTREASURY/YIELD")
names(All) <- c("Date","b","c","d","e","f","g","h","i","j","k","l")

cap <- ggplot(All[All$Date >= "2010-01-01",], aes(y=e, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("US Short-Term Interest Rate (1YR)", All$Date[1]))+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

cap2 <-  ggplot(All[All$Date >= "2010-01-01",], aes(y=c, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("US Short-Term Interest Rate (3MO)")+
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(cap, cap2)
#This last chart shows just the yield on 3-mo T-bills, often referred to as the bedrock risk-free rate for the entire world.
#That this rate has risen from zero to 30 bps
#against the backdrop of a global financial panic in recent weeks is notable, to say the least. 
#If global conditions were truly calamitous, the Fed's efforts to raise short-term rates arguably would have proved futile,
#as there would have been an overwhelming demand for the safety of 3-mo T-bills (Cash equivalents). 

#It's encouraging to see the price of the world's premier safe asset fall (yields rise), 
#since that suggests that the economic and financial market fundamentals have improved on the margin. 
#It also sends a positive message, 
#since it's the Fed's way of saying they have more confidence in the economy's ability to continue growing.

#if the demand for cash and cash-equivalents will decline, 
#it would be manifested chiefly in an increased velocity of money (Credit Creation)
#(money would circulate (credit creation would increase) faster as people attempted to reduce their holdings of money relative to other things), 
#which in turn would cause nominal GDP to accelerate.
#Much of this acceleration in nominal GDP could come from a rising price level (i.e., inflation), 
#and too much of this would present a real challenge to the Fed

#Bottom line, the underpinnings of financial markets look reasonably solid if TED and SWAP spreads are low and
#short-term yields rise
browseURL("http://www.macrotrends.net/1447/ted-spread-historical-chart")


#Very low swap spreads tell us that financial markets are highly liquid and that systemic risk is very low.
#very low swap spreads are an excellent indicator which suggests that systemic risk is low and 
#financial markets are very liquid, and therefore the economic outlook is more likely to improve than not. 
#Stockcharts for TLT:HYG long-term

#But there's one thing missing from the "panic" story. Swap spreads, shown in the chart above, 
#are not displaying any signs of distressLarge institutional investors can buy and sell risk easily, 
#and in quantity, without having to pay high prices to do so. Swap spreads say that systemic risk is low, 
#which is another way of saying that the distress in the HY market 
#and in particular in the current crises has not proved contagious to the larger economy. 

#Systemic risk also fails to appear in the prices of gold and 5-yr TIPS, as the chart above shows. 
#The prices of both of these safe-haven assets have been on a declining trend for several years. 
#If things were really falling apart, the two lines in this chart would be headed skywards. But they're not.




#However, the last 3 rate hikes have been market positive as you can see above. (They all are until it tops out)
#a rate increase means that the economy is improving enough that employment and inflation are considered to be well on the path to being healthy. 
#You would expect, therefore, that stocks would do well if the Fed felt comfortable raising rates. An improving economy also implies demand for commodities and lower default rates, meaning that commodity prices are rising and high yield bonds are at least stable.
#And in fact, this is what usually happens when the Fed raises rates for the first time: stocks and commodities rise and high yield bonds have a positive return over the next year (the average length of time rates rose).

#Although each cycle was different, you can see that equities tend to perform well in rate hikes:
#Page 31: https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top

#When credit is easy and cheap, borrowing and spending will occur; and when it is scarce and expensive,
#borrowing and spending will be less. In the short-term debt cycle, the central bank will control the supply of
#money and influence the amount of credit that the private sector creates by influencing the cost of credit (i.e.,
#interest rates). Changes in private sector credit drive the cycle.

#The short-term debt cycle, also known as the business cycle, is primarily controlled by central banks' policies that
#tighten when inflation is too high and/or rising uncomfortably because there isn't much slack in the economy
#(as reflected in the GDP gap, capacity utilization and the unemployment rate) and credit growth is strong; 
#and ease when the reverse conditions exist.
#http://www.macrotrends.net/1349/a-debt-fueled-stock-market
#http://www.macrotrends.net/1298/the-yield-curve-vs-s-p-500-performance

getSymbols(c("GDPPOT", "GDPC1"), src = "FRED")
fed_cap <- data.frame(merge(GDPPOT, GDPC1))
fed_cap$Date <- as.Date(row.names(fed_cap))
fed_cap$Gap <- (fed_cap$GDPC1-fed_cap$GDPPOT)/fed_cap$GDPPOT
fed_cap <- na.omit(fed_cap)
fed_cap$reg <- lm(fed_cap$Gap~fed_cap$Date)$residuals

cap <- ggplot(fed_cap, aes(y=Gap, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = 0))+
  ylab("")+
  xlab("")+
  ggtitle(paste("GDP Gap", fed_cap$Date[nrow(fed_cap)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

cap2 <-  ggplot(fed_cap, aes(y=reg*10, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = sd(reg*10)), linetype = "dashed")+
  geom_hline(aes(yintercept = -2*sd(reg*10)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("GDP Gap to Trendline")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(cap, cap2)

getSymbols("TCU", src = "FRED")
fed_capa <- data.frame(TCU)
fed_capa$Date <- as.Date(row.names(fed_capa))
fed_capa$reg <- lm(fed_capa$TCU~fed_capa$Date)$residuals

cap <- ggplot(fed_capa, aes(y=TCU, x=Date))+
  geom_line()+
  geom_smooth(method='lm', colour = "black", size = 0.1)+
  ylab("")+
  xlab("")+
  ggtitle(paste("Capacity Utilization", fed_capa$Date[nrow(fed_capa)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

cap2 <-  ggplot(fed_capa, aes(y=reg, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = sd(reg)), linetype = "dashed")+
  geom_hline(aes(yintercept = -2*sd(reg)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("Capacity Utilization to Trend")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(cap, cap2)
#Prosperity exists when the economy is operating at a high level of capacity: in other words, when demand is
#pressing up against a pre-existing level of capacity. At such times, business profits are good and unemployment
#is low. The longer these conditions persist, the more capacity will be increased, typically financed by credit
#growth. Declining demand creates a condition of low capacity utilization; as a result, business profits are bad and
#unemployment is high. The longer these conditions exist, the more cost-cutting (i.e., restructuring) will occur,
#typically including debt and equity write-downs. Therefore, prosperity equals high demand, and in our creditbased
#economy, strong demand equals strong real credit growth; conversely, deleveraging equals low demand,
#and hence lower and falling real credit growth.
#recessions and depressions develop from declines in demand, typically due to a fall-off in credit creation.

getSymbols("TOTCI", src = "FRED")
TOTS <- data.frame(TOTCI)
TOTS$Date <- as.Date(row.names(TOTS))
TOTS$yoy <- Delt(TOTS$TOTCI, k = 52)
#TOTS <- na.omit(TOTS)

tot <- ggplot(TOTS[TOTS$Date >= "1995-01-01",], aes(y=TOTCI, x=Date))+ #Gundlach
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Commercial and Industrial Loans", TOTS$Date[nrow(TOTS)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

tot2 <- ggplot(TOTS[TOTS$Date >= "1995-01-01",], aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Commercial and Industrial Loans")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
grid.arrange(tot, tot2)

#With the demand for loans appearing very weak, bank loan growth will continue to contract, as it has been quite significantly, 
#and this, in turn, will continue to reduce the growth rate in the money supply, lowering overall growth and inflation.
#This should lower long-term interest rates.
#Bank loan growth is contracting across all major categories, and banks are reporting weaker demand for loans. SEE FRED
#This indicates that the contraction in bank loan growth will continue. As bank loan growth contracts, 
#this reduces the growth rate in the money supply (M2) and puts downward pressure on core inflation.

getSymbols("COMPAPER", src = "FRED")
COMPAPER <- data.frame(COMPAPER)
COMPAPER$Date <- as.Date(row.names(COMPAPER))
COMPAPER$yoy <- Delt(COMPAPER$COMPAPER, k = 52)
#COMPAPER <- na.omit(COMPAPER)

tot <- ggplot(COMPAPER, aes(y=COMPAPER, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Nonfinancial Commercial Paper", COMPAPER$Date[nrow(COMPAPER)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

tot2 <- ggplot(COMPAPER, aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Nonfinancial Commercial Paper")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
grid.arrange(tot, tot2)

df <- merge(TOTS, COMPAPER, by = "Date")
df$Value <- df$TOTCI+df$COMPAPER

df$yoy <- Delt(df$Value, k = 52)

tot <- ggplot(df, aes(y=Value, x=Date))+ #Yardini
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Short-Term Business Credit", df$Date[nrow(df)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

tot2 <- ggplot(df, aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Short-Term Business Credit")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
grid.arrange(tot, tot2)

getSymbols(c("INDPRO", "IPMAN"), src = "FRED")
fed_INDPRO <- data.frame(INDPRO)
fed_IPMAN <- data.frame(IPMAN)
fed_INDPRO$Date <- as.Date(row.names(fed_INDPRO))
fed_INDPRO$yoy <- Delt(fed_INDPRO$INDPRO, k = 12)
fed_INDPRO <- na.omit(fed_INDPRO)
fed_IPMAN$Date <- as.Date(row.names(fed_IPMAN))
fed_IPMAN$yoy <- Delt(fed_IPMAN$IPMAN, k = 12)
fed_IPMAN <- na.omit(fed_IPMAN)

ind <- ggplot(fed_INDPRO[fed_INDPRO$Date >= "1990-01-01",], aes(y=INDPRO, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Industrial Production", fed_INDPRO$Date[nrow(fed_INDPRO)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

ind2 <- ggplot(fed_INDPRO[fed_INDPRO$Date >= "1990-01-01",], aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Industrial Production")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(ind, ind2)

ind <- ggplot(fed_IPMAN, aes(y=IPMAN, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Industrial Production: Manufacturing (NAICS)", fed_IPMAN$Date[nrow(fed_IPMAN)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

ind2 <- ggplot(fed_IPMAN, aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Industrial Production: Manufacturing (NAICS)")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))


grid.arrange(ind, ind2)

getSymbols("PAYEMS", src = "FRED")
fed_pi <- data.frame(PAYEMS)
fed_pi$Date <- as.Date(row.names(fed_pi))

fed_pi$yoy <- Delt(fed_pi$PAYEMS, k = 12)
fed_pi <- na.omit(fed_pi)

indpay <- ggplot(fed_pi[fed_pi$Date >= "1990-01-01",], aes(y=PAYEMS, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Total Nonfarm Employees", fed_pi$Date[nrow(fed_pi)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

ind2pay <- ggplot(fed_pi[fed_pi$Date >= "1990-01-01",], aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = mean(fed_pi$yoy), color = "blue", linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Total Nonfarm Employees")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
grid.arrange(indpay, ind2pay)

getSymbols(c("RSAFS","PCEPILFE"), src = "FRED")
fed_pi <- data.frame(merge(RSAFS,PCEPILFE))
fed_pi$Date <- as.Date(row.names(fed_pi))

fed_pi$yoy <- Delt((fed_pi$RSAFS/fed_pi$PCEPILFE), k = 12)
fed_pi <- na.omit(fed_pi)

ind <- ggplot(fed_pi, aes(y=RSAFS/PCEPILFE, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Real Retail Sales", fed_pi$Date[nrow(fed_pi)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

ind2 <- ggplot(fed_pi, aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = mean(yoy)), color = "blue", linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("YoY % Change in Real Retail Sales")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
grid.arrange(ind, ind2)


getSymbols(c("CIVPART", "UNEMPLOY","CE16OV"), src = "FRED")
fed_pi <- data.frame(merge(CIVPART, UNEMPLOY,CE16OV))
fed_pi$Date <- as.Date(row.names(fed_pi))

civ <- ggplot(fed_pi, aes(y=CIVPART, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Civilian Labor Force Participation Rate", fed_pi$Date[nrow(fed_pi)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

other <- ggplot(fed_pi, aes(y=Delt(UNEMPLOY+CE16OV, k = 12), x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("Annual change in the labor force (employment plus unemployment)")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(civ, other)

#Since the early 2000s, labor force participation has been declining in the U.S.  
#the labor force participation rate declined consistently to 62.4 percent in September 2015 and has since flattened out. 
#The first graph shows the period of decline in the labor force participation rate, which started in early 2000, 
#flattened out in mid-2005, and then declined again from the onset of the Great Recession to 2015.

#The next graph shows the annual change in the labor force (employment plus unemployment). 
#While the labor force has mostly been increasing since 2000, 
#it has not been increasing fast enough to keep up with population growth. Starting in 2014, however, 
#the pace of growth in the labor force picked up, which led to the flattening out of the participation rate.

#Unemployment: http://www.macrotrends.net/1316/us-national-unemployment-rate
#Average is 6,2%
getSymbols("UNRATE", src = "FRED")
UNRATE <- data.frame(UNRATE)
UNRATE$Date <- as.Date(row.names(UNRATE))
UNRATE$ma <- SMA(UNRATE$UNRATE, 12)
#Crossing of 12 ma with unemp rate is a warning - Gundlach
ggplot(UNRATE, aes(y=UNRATE, x=Date))+
  geom_line()+
  geom_line(aes(y=ma, x=Date), col = "red", alpha = 0.3)+
  geom_hline(aes(yintercept = mean(UNRATE)))+
  geom_hline(aes(yintercept = mean(UNRATE)-sd(UNRATE)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(UNRATE)+2*sd(UNRATE)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Unemployment Rate", UNRATE$Date[nrow(UNRATE)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
#http://www.macrotrends.net/1371/retail-sales-historical-chart

#Claims:
#Unemployment claims are also in a declining trend; historically, 
#claims have started to rise at least 6 months ahead of the next recession.
getSymbols(c("CCSA", "IC4WSA"), src = "FRED")
fed_UNRATE <- data.frame(merge(CCSA, IC4WSA))
fed_UNRATE$Date <- as.Date(row.names(fed_UNRATE))

ccsa_p <- ggplot(fed_UNRATE, aes(y=CCSA, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(CCSA, na.rm = T)))+
  geom_hline(aes(yintercept = mean(CCSA, na.rm = T)-sd(CCSA, na.rm = T)), color = "blue", linetype = "dashed")+
  geom_hline(aes(yintercept = mean(CCSA, na.rm = T)+2*sd(CCSA, na.rm = T)), color = "red", linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("4-Week Moving Average of Initial Claims", fed_UNRATE$Date[nrow(fed_UNRATE)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

ic_p <- ggplot(fed_UNRATE, aes(y=IC4WSA, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(IC4WSA, na.rm = T)))+
  geom_hline(aes(yintercept = mean(IC4WSA, na.rm = T)-sd(IC4WSA, na.rm = T)), color = "blue", linetype = "dashed")+
  geom_hline(aes(yintercept = mean(IC4WSA, na.rm = T)+2*sd(IC4WSA, na.rm = T)), color = "red", linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("Continued Claims (Insured Unemployment)")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(ccsa_p, ic_p)
#However you look at the data, claims have reached a new low for the current business cycle.
#This is consistent with the pattern of every business cycle. This recovery has been miserably slow, but it is ongoing.
#As the above chart shows, equity prices have been tracking the improvement in claims throughout this recovery. 
#Equities offer returns that are too attractive to resist as long as the economy fails to deteriorate, 
#especially when compared to the extremely low yields on cash equivalents and Treasuries.
getSymbols("AHETPI", src = "FRED")
fed_AHETPI <- data.frame(AHETPI)
fed_AHETPI$Date <- as.Date(row.names(fed_AHETPI))
fed_AHETPI$yoy <- Delt(fed_AHETPI$AHETPI, k = 12)

ggplot(fed_AHETPI, aes(y=yoy, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(yoy, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("YoY growth in wages of production and non-supervisory workers", fed_AHETPI$Date[nrow(fed_AHETPI)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))
#Doesn't look like wage growth has reached it's peak for the cycle though and unemployment can go down even more.

#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#page 20: Growth in workers and growth in real output per worker equals real GDP growth. 
#The U.S. will grow at a slower rate this decade than in previous ones 
#because of our limited capacity in new workers and productivity increases. 

getSymbols(c("ISRATIO","ALTSALES","HOUST","WPSFD41312","NEWORDER"), src = "FRED", from = "1995-01-01")
fed_cyclicals <- data.frame(merge(ISRATIO,ALTSALES,HOUST,WPSFD41312,NEWORDER))
fed_cyclicals$Date <- as.Date(row.names(fed_cyclicals))
fed_cyclicals <- fed_cyclicals[fed_cyclicals$Date>="1995-01-01",]

cycl1 <- ggplot(fed_cyclicals, aes(y=ISRATIO, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Manufacturing and Trade Inventories")+
  scale_x_date(date_breaks = "4 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

cycl2 <-  ggplot(fed_cyclicals, aes(y=ALTSALES, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(ALTSALES)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("Light Vehical Sales")+
  scale_x_date(date_breaks = "4 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

cycl3 <- ggplot(fed_cyclicals, aes(y=HOUST, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(HOUST)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Housing Starts", fed_cyclicals$Date[nrow(fed_cyclicals)]))+
  scale_x_date(date_breaks = "4 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

cycl4 <- ggplot(fed_cyclicals, aes(y=NEWORDER/WPSFD41312, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(NEWORDER/WPSFD41312, na.rm = TRUE)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("Real Manufacturers' New Orders")+
  scale_x_date(date_breaks = "4 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(cycl3, cycl2, cycl1, cycl4)
browseURL("http://www.macrotrends.net/2601/sp-500-vs-durable-goods-chart")
#These are the four most cyclical areas of the U.S. economy, 
#which have historically been responsible for driving both expansions and recessions. - J.P. Morgan AM
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#Page 18: The four charts on this slide show the four most cyclical areas of the U.S. economy, 
#which have historically been responsible for driving both expansions and recessions.
#http://www.macrotrends.net/1314/housing-starts-historical-chart
#http://www.macrotrends.net/1372/auto-and-light-truck-sales-historical-chart

ism.comp <- Quandl("ISM/MAN_PMI", collapse = "monthly", trim_start = "2000-01-01")
ism.comp_emp <- Quandl("ISM/MAN_EMPL", collapse = "monthly", trim_start = "2000-01-01")
ism.comp_ord <- Quandl("ISM/MAN_EXPORTS", collapse = "monthly", trim_start = "2000-01-01")

#Correlates with growth of Sales per share (revenue) for sp500
ism_p1 <- ggplot(ism.comp, aes(y=ism.comp[,2], x=Date)) +
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("ISM Composite", as.Date(ism.comp$Date)[1]))+
  geom_hline(yintercept = 50)+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

ism_p2 <- ggplot(ism.comp_emp, aes(y=ism.comp_emp[,2], x=Date)) +
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("ISM Employment Index")+
  #geom_hline(yintercept = 50)+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

ism_p3 <- ggplot(ism.comp_ord, aes(y=ism.comp_ord[,2], x=Date)) +
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("ISM New Export Orders Index")+
  #geom_hline(yintercept = 50)+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(ism_p1,ism_p2,ism_p3)

#Global and EU ISM:
browseURL("https://www.yardeni.com/pub/ecoindglpmimfg.pdf")

#Based on past correlations, the strength of the ISM manufacturing index says that revenues per share of major US corporations are likely to remain strong for the foreseeable future. 
#Coupled with near-record profits, this provides a reasonable basis for a continuing rally in equity prices.
#ISM down is real GDP growth down. ISM leads.
#ISM leads sp500 12 month change
#http://www.multpl.com/us-real-gdp-growth-rate
#it suggests that economic growth in the current quarter will remain disappointingly slow. Deteriorating.
#A reading above 50 indicates that the manufacturing economy is generally expanding.
#Under 50 and it is contracting.

#But he said the economic leading indicators are positive and not looking recessionary. 
#According to Gundlach, there has never been a recession without the leading indicators first going into negative territory. 
#He added that the ISM indicators were looking “quite robust.”

#GS: typically very high levels of momentum indicators, such as the ISM and PMIs, tend to be followed by lower returns when the pace of growth starts to moderate. 
#The highest returns are when the ISM is low but recovering, while the lowest are when it is low and deteriorating.

getSymbols(c("GDPC1","GDP"), src = "FRED")
fed_gdp <- data.frame(merge(GDPC1,GDP))
fed_gdp$Date <- as.Date(row.names(fed_gdp))

fed_gdp$realyoy <- Delt(fed_gdp$GDPC1, k = 4)
fed_gdp$nomyoy <- Delt(fed_gdp$GDP, k = 4)

realyoy <- ggplot(fed_gdp, aes(y=realyoy, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Real GDP Growth")+
  geom_hline(aes(yintercept = mean(realyoy, na.rm = T)), color = "blue", linetype = "dashed")+
  geom_hline(aes(yintercept = 0))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

nomyoy <-  ggplot(fed_gdp, aes(y=nomyoy, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(nomyoy, na.rm = T)), color = "blue", linetype = "dashed")+
  geom_hline(aes(yintercept = 0))+
  ylab("")+
  xlab("")+
  ggtitle("Nominal GDP Growth")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

grid.arrange(realyoy, nomyoy)
#Economic Growth. We are below long-term avg (blue line). Worst recovery in history.
#"early-cycle": (i.e., in excess of 4%)
#"mid-cycle": (i.e., to around 2%)
#"late-cycle": (i.e., around 3.5-4%)
#These are measure of productivity!
#Reminder: 10 Year Yields on Bonds are market expectations of nominal GDP growth.


#Strong dollar is capital inflow. However it does hurt corporate profits.
#Strong trade weighted US dollar is inflow of capital. Demand for dollar denominated goods and in search of yields.
#However, a strong dollar is clearly weighing on exports.

#A stronger US economy is causing money flow to US and thus strenghtening the dollar
#Behind these moves is a U.S. economy that appears healthier than the Eurozone economy, 
#and that in turn conditions markets to expect very different monetary policy conditions for the foreseeable future. 
#The dollar's strength to date is salutary and not overdone, so it doesn't present a problem for the U.S. economy.


#In the tightening phase of the expansion, actual or anticipated
#acceleration of inflation prompts the Fed to turn restrictive, which shows up in reduced liquidity, interest rates rising
#and the yield curve flattening or inverting.
#This, in turn, causes money supply and credit growth to fall and the stock market to decline before the economy turns down.

#Recessions have always followed the confluence of high and rising real interest rates and flat to inverted yield curves.

#The chart above compares the real yield on Fed funds to the slope of the nominal yield curve. 
#Note that the yield curve is typically positively sloped in the early stages of a business cycle recovery, 
#but that it becomes negatively sloped in the latter stages. 
#Why? Because inflation has typically picked up as the business cycle matures, http://www.macrotrends.net/1466/inflation-rates-100-year-historical-chart
#and the Fed typically uses tighter money policy (which takes the form of higher real yields) in order to "cool off" 
#the expansion and suppress inflation pressures. 
#Every recession in the past 50 years has been preceded by a significant rise in real short-term yields. 
#The shape of the yield curve today and the low level of real yields tell us that we are probably years 
#away from another recession, because the market doesn't expect any aggressive tightening from the Fed for many years.
#Today, FED rates are still low and the yield curve is still positively-sloped. 
#Monetary policy thus appears to pose no threat to economic growth, at least as far as the bond market is concerned.
#during each rate hiking cycle, the yield curve has flattened.

#Yield Curve leads


getSymbols("FF", src = "FRED")
FFY <- data.frame(FF)
FFY$Date <- as.Date(row.names(FFY))
All <- Quandl("USTREASURY/YIELD", collapse = "daily")
names(All) <- c("Date","b","c","d","e","f","g","h","i","Ten","k","l")

Allm <- merge(FFY, All[,c("Date", "Ten")])

ffy1 <- ggplot(Allm, aes(y=Ten, x=Date))+
  geom_line()+
  geom_hline(yintercept = 5)+
  ylab("")+
  xlab("")+
  ggtitle(paste("US 10-Year Treasury Yield", as.Date(All$Date)[1]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

ffy2 <- ggplot(Allm, aes(y=FF, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Effective Fed Funds Rate")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(ffy1, ffy2, ncol = 1)
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#Page 12: When 10-Year yields are below 5%, rising rates are generally associated with rising stock prices

browseURL("https://research.stlouisfed.org/useraccount/fredgraph/?graph_id=421621")
#rising rates indicate an economy that is strong, with expanding growth, incomes, consumption and profits. 
#Under these circumstances, equities should rise.


#Low real rates are good (lower the better) but the market is pricing in slow growth and low inflation.
#both Treasury and TIPS yields are very low today-despite being unusually low relative to inflation-
#because the market is convinced that the prospects for U.S. economic growth are dismal, and the market is willing to pay a very high
#price (in the form of very low yields) for the safety of Treasuries.
#http://www.macrotrends.net/1281/cpi-vs-ppi-historical-chart

#http://www.macrotrends.net/1281/cpi-vs-ppi-historical-chart
#It seems clear that 10-yr yields, charted above, are unusually low relative to current inflation, in the link. 
#The rest of the charts make it very clear that inflation expectations are on the rise and that at this point there is no reason
#to believe that inflation will fall. If the gap in the first chart is unsustainable, and I think it is, 
#then it is thus very likely to close with 10-yr yields moving higher.
#Higher yields will thus be symptomatic of an improving economic outlook and rising investor confidence. 

#http://www.multpl.com/30-year-treasury-rate/
#http://www.macrotrends.net/1281/cpi-vs-ppi-historical-chart
#What does stand out, however, is the unusually low level of T-bond yields relative to inflation. 
#The chart above is structured to show that 30-yr bond yields over very long periods have averaged about 2.5% more than core inflation. 
#If that long-term average condition were to prevail today, 30-yr T-bond yields would be trading around 4.8%; instead they are 2.7%. 
#Bond yields were arguably fairly valued for much of the decade of the 2000's. But in the past year or so, 
#bond yields have fallen while inflation has risen, and 30-yr bond yields today are only slightly higher than the average inflation
#rate over the past decade. Thus, bonds arguably are richly valued today.
#Low yields on Treasuries are thus an excellent indicator of how bearish the market is. Remember that yields are growth expectations.

#This, I would argue, coupled with the market's generally high level of risk aversion 
#which can be found in the spike in gold, negative real yields on TIPS, the relatively low level of equity PEs
#offers a much more robust explanation for why Treasury yields are so low today. 
#The market is scared, and confidence in the economy's ability to generate stronger growth is very weak. 
#And why should it be stronger, considering the early expansion is usually the best, and it has been very poor as shown in the
#YoY % growth of GDP


#Real yields on 5-yr TIPS today tipsY$TIPSY05[1] can be thought of  
#as the market's expectation for the average real yield on Fed funds over the next 5 years. 
#Rising real yields on TIPS are also a sign that the market is becoming more confident in the ability of the economy to grow.
#real yields on TIPS tending to be a point or so less than the economy's growth tendency over the past two years. 
#real yields tend to track the economy's real growth rate. Both have been rising a bit in recent years.
#http://www.multpl.com/us-real-gdp-growth-rate
#http://www.tradingeconomics.com/united-states/gdp-growth
#stocks will perform best during periods of growth. 
#Owning the traditional, equity heavy portfolio is akin to taking a huge bet on stocks and, 
#at a more fundamental level, that growth will be above expectations.
#equity holdings are exposed to the risk that growth in the economy would be less than discounted by the market.
#Real yields on 5-yr TIPS, arguably a good proxy for the market's perception of the economy's underlying growth potential,
#have risen from a low of -1.8% in early 2013 to almost 0.5% today. 
#I note that this increase in real yields has tended to track an improvement in the economy's growth rate over the same period.
#Growth expectations are still modest, but they are improving on the margin, and not deteriorating. 

#the extremely low (actually negative) level of real yields on TIPS tells us that the bond market is very pessimistic 
#about the prospects for economic growth.
#The chart is suggesting that TIPS are priced to the expectation that real economic growth will be zero at best for the next two years
#and that would undoubtedly lead to some very negative real returns on equities and corporate bonds.


#however, real rates(5year tips) have been rallying because growth expectations are rising
#rates have been flat in 5 year nominal bonds but
#nominal yields especially in the case of the 10year have fallen because inflation expectations have fallen
#stronger growth and lower inflation expectations is a terrific combination


#The chart above shows the market's expectation for the average annual inflation rate(Headline CPI) over the next five years, 
#as derived from the yields on 5-yr Treasuries and 5-yr TIPS.  
#Currently 1.32%, expected inflation is lower than the current level of core inflation, 
#http://www.macrotrends.net/1281/cpi-vs-ppi-historical-chart
#http://www.macrotrends.net/1466/inflation-rates-100-year-historical-chart
#and lower than the 2% average inflation rate of the past 15 years. 
#If anything, this suggests the market may be underestimating inflation. 
#If so, then it would stand to reason that the bond market is quite vulnerable to any sign of rising inflation.
#Projected inflation rate: http://www.statista.com/statistics/244983/projected-inflation-rate-in-the-united-states/


#http://www.macrotrends.net/1373/oil-prices-vs-the-cpi-historical-chart
#On Core inflation and Headline: Since crude prices started falling in the summer of 2014, 
#headline inflation fell from 2% to essentially zero for most of this year. 
#Yet if we exclude energy from the calculation, inflation has been fairly steady at 2% for a long time.

#The PCE Price Index, which is the FED's preferred reading on inflation.
#personal consumption expenditures accounts for 68 percent of total GDP.

#In the tightening phase of the expansion the actual or anticipated
#acceleration of inflation prompts the Fed to turn restrictive, which shows up in reduced liquidity.

#very low inflation is a sign that monetary policy is too tight and threatens widespread corporate bond defaults. 
#The reality, however, is quite different. As I've been pointing out for a long time, 
#if you strip out energy prices you find that the underlying rate of consumer price inflation has been running at a 2% pace for more than a decade. 
#Nothing has changed of late. And there's no reason to think the current rate of inflation is too low.
#Headline inflation collapsed, but bounced back once oil prices stopped declining.
#I note that in the late 1990s inflation was about the same as it is today (about 2%) yet that was a period of very strong growth (about 4% a year for several years).
#Low and stable inflation is a good thing. 
#As an aside, I should note that the "core" rate of consumer price inflation is up at a 2.3% annualized pace over the past six months. 
#Lower oil prices may be helping other prices to rise, because consumers have more money to spend on other things.


#This last chart compares 5-yr forward inflation expectations to the level of the S&P 500, 
#which is the market's expectation for the annualized rate of CPI inflation over the next 5 years.
#Both inflation expectations and equity prices have moved together over the past 15 and also for the past 27 months, 
#which suggests that the Fed's accommodative policy stance at the very least is driving the market's outlook for nominal growth. 
#Both equities and bonds are assuming faster nominal growth-driven by higher inflation(rising line).
#Look at this as a longer-term thing and short-term as well.
#Compare it to the current Headline CPI inflation: http://www.macrotrends.net/1466/inflation-rates-100-year-historical-chart
#http://www.macrotrends.net/1281/cpi-vs-ppi-historical-chart
#to see if we expect inflation to go up or down.

#the value of any investment is primarily determined by the volume of economic activity (growth) and its pricing (inflation).
#asset volatility will be largely driven by how economic conditions unfold relative to current expectations

#The above chart compares the S&P 500 index to the 5-yr, 5-yr forward expected inflation rate embedded in TIPS and Treasury prices 
#(which happens to be the Fed's preferred measure of inflation expectations). 
#There is a noticeable tendency for equity prices to rise as inflation expectations heat up, 
#and to decline as inflation expectations cool off

#If inflation does indeed move higher, in line with expectations, then sooner or 
#later Treasury yields are going to have to move higher. Higher Treasury yields-and higher interest rates in general-
#won't be a problem for the economy or for the stock market, since they will be the natural result of a stronger economy and
#/or faster nominal growth.

#Ray Dalio:
#Rising Growth Expectations
#   Stocks
#   Commodities
#   Corporate Bonds
#   Emerging Market Bonds

#Falling Growth Expectations
#   Treasuries
#   TIPS / Inflation Linked Bonds

#Rising Inflation Expectations
#   TIPS / Inflation Linked Bonds
#   Commodities
#   Emerging Market Bonds

#Falling Inflation Expectations
#   Stocks
#   Treasuries


Big8 <- Quandl("UMICH/SOC1", trim_start = "1988-01-01")
OIL <- Quandl("FRED/WCOILWTICO", collapse="monthly", trim_start = "1988-01-01")
names(OIL) <- c("Date", "Value")

OIL8 <- merge(Big8, OIL, by = "Date")

oil1 <- ggplot(OIL8, aes(y=-1*log(Value), x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Inverted Oil Prices", as.Date(OIL8$Date)[nrow(OIL8)]))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

oil2 <- ggplot(OIL8, aes(y=Index, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Consumer Sentiment Index")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

oil3 <- ggplot(OIL8, aes(y=SMA(Index, n = 5), x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("5 Month Moving Average of Sentiment")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(oil1,oil2,oil3, ncol = 1)

browseURL("https://www.yardeni.com/pub/stmktbullbear.pdf")
#page 1 and 7 fig 11

#The chart above compares the real price of oil (inverted) to a measure of consumer confidence. 
#Cheaper oil in the past years has helped boost consumer confidence considerably.
#As Oil cheapens, consumers gain confidence.
#Strong consumer sentiment is good! Not if it is overextended though.
#Not surprisingly, as the chart above shows, consumer confidence has been rising since 2011, 
#along with the fall in gold and TIPS prices, and the rise in stock prices.

#Low spikes have had a larger subsequent 12-month S&P 500 return than spikes higher.
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#page 28

#tight money (Fed Funds up) and high and rising oil prices conspired to trigger recessions
#as the economy gets "squeezed" by expensive money and expensive energy


#Strengthening dollar is depressing commodity prices including oil prices, which is strengthening the dollar, again.
#lower oil prices are boosting global growth.
#The strong dollar's negative impact is visible in the (M-)PMI's new exports component

OIL <- Quandl("FRED/WCOILWTICO", collapse = "weekly", trim_start = "1980-01-01")
Gold <- Quandl("WGC/GOLD_DAILY_USD", collapse = "weekly", trim_start = "1980-01-01", trim_end = head(OIL$Date,1))
names(OIL) <- c("Date", "Value")
names(Gold) <- c("Date", "Value")
OilG <- merge(OIL, Gold, by = "Date")

ggplot(OilG, aes(y=OilG$Value.x/OilG$Value.y, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle(paste("Oil to Gold", as.Date(OilG$Date)[nrow(OilG)]))+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
browseURL("http://www.macrotrends.net/1369/crude-oil-price-history-chart")
#If tight money and expensive oil have traditionally been bad for the economy, then today's negative real rates and 
#cheap oil prices are likely a godsend. This suggests that the economy can do better than the market expects
#http://www.macrotrends.net/1369/crude-oil-price-history-chart
#All down moves have been characterized by Commodity Spikes, Aggressive Fed or Extreme Valuations. None are present now.
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#page 14

#The chart above compares the inverse of real yields on 5-yr TIPS (using that as a proxy for their price) 
#to the price of gold. It's rather remarkable that the two have tracked each other so well for the past 8 years, 
#since these two assets share almost nothing in common. The one thing they do share, however, 
#is that they are both considered to be safe-haven assets. Gold is the favored port in any economic or financial storm, 
#while 5-yr TIPS are not only risk-free but also inflation-protected. So the fact that they are moving together suggests 
#that what is acting on these two prices is the market's degree of risk aversion. Today we see less risk aversion, 
#because the prices of gold and TIPS have fallen.The market thus seems to be transitioning from a period of high 
#risk-aversion to lower risk aversion. 
#As a corollary we could say that optimism was in very short supply a few years ago and is now beginning to return. 
#But we are still far from a market which is "irrationally exuberant." 
#With the benefit of hindsight, the market was excessively confident when gold approached $250/oz and TIPS yields were 4%, 
#in the 2000-2001 period. 


browseURL("http://www.macrotrends.net/1437/gold-to-s-p-500-ratio")
#The chart above shows the ratio of the S&P 500 index to gold prices, and it is divided into periods
#in which the ratio was rising and falling. 
browseURL("http://www.multpl.com/us-gdp-growth-rate")
#Growth has been slower during times when gold is outperforming stocks(ratio falling), and faster when gold is underperforming. 
#The ratio has been trending up for several years now, suggesting that growth could pick up further in the years to come. 
#Investors tend to prefer gold during times of great uncertainty, 
#and they tend to shun gold during times of rising confidence. 
#Think of the green periods as times when investors run away from financial assets and embrace hard assets, 
#and the white periods as times when financial assets look more attractive than hard assets. 
#Good times for financial assets mean investment is generally strong, and investment is ultimately what drives growth.
#A low ratio signifies low confidence (gold high, S&P low) and a high ratio signals a high confidence (S&P high, gold low).

tipsY <- Quandl("FED/TIPSY", collapse = "monthly")
PE <- Quandl("MULTPL/SP500_PE_RATIO_MONTH")
Shiller <- Quandl("MULTPL/SHILLER_PE_RATIO_MONTH")

tipspe <- merge(PE, Shiller, by = "Date")
tipspe$Date <-format(as.Date(tipspe$Date), "%Y-%m")
tipsY$Date <-format(as.Date(tipsY$Date), "%Y-%m")
tipspeY <- merge(tipspe, tipsY, by = "Date")

tipspeY$Date <- as.Date(as.yearmon(tipspeY$Date))


sp <- ggplot(tipspeY, aes(y=TIPSY05, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("5Y Real Yields - Growth Expectations Monthly", as.Date(tipspeY$Date)[nrow(tipspeY)]))+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

sp1 <- ggplot(tipspeY, aes(y=log(Value.x), x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("S&P500 PE Logarithmic")+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

sp2 <- ggplot(tipspeY, aes(y=Value.y, x=Date))+
  geom_line()+
  ylab("")+
  xlab("")+
  ggtitle("Shillers PE")+
  theme_bw()+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(sp, sp1, sp2, ncol = 1)

browseURL("http://www.macrotrends.net/1324/s-p-500-earnings-history")

#See stockcharts !PESPX
#The chart above compares real yields on 5-yr TIPS to the PE ratio of the S&P 500. 
#Here again we see an interesting correlation, with the exception of the 2004-2007 period. 
#This period was one in which the Fed was aggressively tightening monetary policy, 
#which involves forcing real short-term interest rates higher. When the Fed is not forcibly intervening in the market, 
#real yields show a strong tendency to track PE ratios.
#What does this tell us? We know that rising PE ratios tend to correlate to a rising tolerance for risk and 
#increased optimism about the future of the economy. Investors are willing to pay more for a dollar's worth of earnings 
#when they believe the economy-and profits-are likely to improve. 
#Today, PE ratios are only slightly above their long-term average. 
#This confirms the message of gold and TIPS, which is that the market is transitioning from being very afraid to becoming 
#cautiously confident. Valuations, in other words are somewhere between cheap and expensive.
#The economy is growing at a sub-par rate, the market is cautiously optimistic
#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#Page 5: PE measures
#Page 6: These charts demonstrate the historical relationship between forward P/E levels and subsequent 1 and 5 year returns. 
#The higher the PE, the lower the return


#TigerTrade: http://www.multpl.com/s-p-500-earnings-yield and http://www.multpl.com/10-year-treasury-rate
#Caution is also reflected in the chart above, which shows the difference between the earnings yield on stocks and 
#the yield on 10-yr Treasuries. In order to satisfy investors' aversion to risk, 
#the market demands a yield on stocks that is about 4% higher than the yield on safe Treasuries. 
#Today's equity risk premium is substantially higher than its long-term average.
browseURL("http://www.multpl.com/s-p-500-earnings-yield/table/by-month")-browseURL("http://www.multpl.com/10-year-treasury-rate/table/by-month")

#a bit longer-term valuation perspective:
Shiller <- Quandl("MULTPL/SHILLER_PE_RATIO_MONTH")
getSymbols(c("MVEONWMVBSNNCB", "TNWMVBSNNCB"), src = "FRED")
Tobin <- data.frame(merge(MVEONWMVBSNNCB,TNWMVBSNNCB))
Tobin$Date <- as.Date(row.names(Tobin))

shill <- ggplot(Shiller, aes(y=Value, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(Value)))+
  geom_hline(aes(yintercept = mean(Value)+sd(Value)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(Value)+2*sd(Value)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(Value)-1*sd(Value)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Cyclically-Adjusted P/E", as.Date(Shiller$Date)[1]))+
  scale_x_date(date_breaks = "15 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

tob <- ggplot(Tobin, aes(y=MVEONWMVBSNNCB/TNWMVBSNNCB, x=Date))+
  geom_line()+
  geom_hline(aes(yintercept = mean(MVEONWMVBSNNCB/TNWMVBSNNCB, na.rm = T)))+
  geom_hline(aes(yintercept = mean(MVEONWMVBSNNCB/TNWMVBSNNCB, na.rm = T)+sd(MVEONWMVBSNNCB/TNWMVBSNNCB, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(MVEONWMVBSNNCB/TNWMVBSNNCB, na.rm = T)+2*sd(MVEONWMVBSNNCB/TNWMVBSNNCB, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(MVEONWMVBSNNCB/TNWMVBSNNCB, na.rm = T)-1*sd(MVEONWMVBSNNCB/TNWMVBSNNCB, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Tobin's Q", as.Date(Tobin$Date)[nrow(Tobin)]))+
  scale_x_date(date_breaks = "8 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(shill, tob, ncol = 1)

getSymbols(c("NCBEILQ027S", "WILL5000PRFC", "GDP"), src = "FRED")
df_1 <- data.frame(merge(NCBEILQ027S, GDP))
df_2 <- data.frame(merge(WILL5000PRFC, GDP))
df_1$Date <- as.Date(row.names(df_1))
df_2$Date <- as.Date(row.names(df_2))

for(i in 2:nrow(df_2)){
  if(is.na(df_2$WILL5000PRFC[i])){
    df_2$WILL5000PRFC[i] <- df_2$WILL5000PRFC[i-1]
  }
}

a <- ggplot(na.omit(df_1), aes(y=NCBEILQ027S/GDP, x=Date))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept = mean(NCBEILQ027S/GDP, na.rm = T)))+
  geom_hline(aes(yintercept = mean(NCBEILQ027S/GDP, na.rm = T)+sd(NCBEILQ027S/GDP, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(NCBEILQ027S/GDP, na.rm = T)+2*sd(NCBEILQ027S/GDP, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(NCBEILQ027S/GDP, na.rm = T)-1*sd(NCBEILQ027S/GDP, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Corporate Equities to GDP", as.Date(df_1$Date)[nrow(df_1)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

b <- ggplot(na.omit(df_2), aes(y=WILL5000PRFC/GDP, x=Date))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept = mean(WILL5000PRFC/GDP, na.rm = T)))+
  geom_hline(aes(yintercept = mean(WILL5000PRFC/GDP, na.rm = T)+sd(WILL5000PRFC/GDP, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(WILL5000PRFC/GDP, na.rm = T)+2*sd(WILL5000PRFC/GDP, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(WILL5000PRFC/GDP, na.rm = T)-1*sd(WILL5000PRFC/GDP, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Market Capitalization to GDP", as.Date(df_2$Date)[nrow(df_2)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
grid.arrange(a, b, ncol = 1)


#Shiller <- Quandl("MULTPL/SHILLER_PE_RATIO_MONTH")

Shiller$percentile <- ecdf(Shiller$Value)(Shiller$Value)
Shiller <- Shiller[order(as.Date(Shiller$Date), decreasing = TRUE),]

getSymbols(c("MVEONWMVBSNNCB", "TNWMVBSNNCB"), src = "FRED")
Tobin <- data.frame(merge(MVEONWMVBSNNCB,TNWMVBSNNCB))
Tobin$Value <- Tobin$MVEONWMVBSNNCB/Tobin$TNWMVBSNNCB
Tobin$percentile <- ecdf(Tobin$Value)(Tobin$Value)
Tobin <- Tobin[order(as.Date(row.names(Tobin)), decreasing = TRUE),]

SPYmultpl <- Quandl("MULTPL/SP500_INFLADJ_MONTH")
SPYmultpl$Value <- log(SPYmultpl$Value)
SPYmultpl$percentile <- ecdf(lm(SPYmultpl$Value~SPYmultpl$Date)$residuals)(lm(SPYmultpl$Value~SPYmultpl$Date)$residuals)

sp <- ggplot(SPYmultpl, aes(y=Value, x=Date))+
  geom_line()+
  geom_smooth(method='lm', colour = "black", size = 0.1)+
  ylab("")+
  xlab("")+
  ggtitle(paste("S&P500 Inflation Adjusted Logarithmic Price Composite Index - Monthly", as.Date(SPYmultpl$Date)[1]))+
  scale_x_date(date_breaks = "20 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

sp1 <- ggplot(SPYmultpl, aes(y=lm(Value ~ Date)$residuals, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = sd(lm(Value ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = 2*sd(lm(Value ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -1*sd(lm(Value ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -2*sd(lm(Value ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("Inflation Adjusted Market Performance to Trend - Monthly")+
  scale_x_date(date_breaks = "20 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(sp, sp1)


getSymbols("^GSPC", from = "1950-01-01")
SPYGSPC <- data.frame(GSPC)
SPYGSPC <- na.omit(SPYGSPC)
SPYGSPC$Date <- as.Date(row.names(SPYGSPC))
SPYGSPC <- SPYGSPC[order(as.Date(SPYGSPC$Date), decreasing = TRUE),]
SPYGSPC$GSPC.Adjusted <- log(SPYGSPC$GSPC.Adjusted)
SPYGSPC$percentile <- ecdf(lm(SPYGSPC$GSPC.Adjusted~SPYGSPC$Date)$residuals)(lm(SPYGSPC$GSPC.Adjusted~SPYGSPC$Date)$residuals)


sp <- ggplot(SPYGSPC, aes(y=GSPC.Adjusted, x=Date))+
  geom_line()+
  geom_smooth(method='lm', colour = "black", size = 0.1)+
  ylab("")+
  xlab("")+
  ggtitle(paste("S&P500 Logarithmic Price Composite Index - Daily", as.Date(SPYGSPC$Date)[nrow(SPYGSPC)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

sp1 <- ggplot(SPYGSPC, aes(y=lm(GSPC.Adjusted ~ Date)$residuals, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = sd(lm(GSPC.Adjusted ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = 2*sd(lm(GSPC.Adjusted ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -1*sd(lm(GSPC.Adjusted ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -2*sd(lm(GSPC.Adjusted ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("Long-Term Market Performance to Trend - Daily")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(sp, sp1)

SPYmultpl$Value <- lm(SPYmultpl$Value~SPYmultpl$Date)$residuals
SPYmultpl <- SPYmultpl[order(as.Date(SPYmultpl$Date), decreasing = TRUE),]
SPYGSPC$Value <- lm(SPYGSPC$GSPC.Adjusted~SPYGSPC$Date)$residuals
Shiller$name <- "P/E10"
Tobin$name <- "Tobins Q"
SPYGSPC$name <- "SPY from Regression (Nominal)"
SPYmultpl$name <- "SPY from Regression (Real)"
df_1$name <- "Corporate Equities to GDP"
df_1$percentile <- ecdf(df_1$NCBEILQ027S/df_1$GDP)(df_1$NCBEILQ027S/df_1$GDP)
df_1$Value <- df_1$NCBEILQ027S/df_1$GDP
df_1 <- df_1[order(as.Date(df_1$Date), decreasing = TRUE),]

df_2$name <- "Market Capitalization to GDP"
df_2$percentile <- ecdf(df_2$WILL5000PRFC/df_2$GDP)(df_2$WILL5000PRFC/df_2$GDP)
df_2$Value <- df_2$WILL5000PRFC/df_2$GDP
df_2 <- df_2[order(as.Date(df_2$Date), decreasing = TRUE),]

getSymbols(c("GDP", "CP", "CPATAX"), src = "FRED")
total1 <- data.frame(merge(GDP,CP,CPATAX))
total1$Date <- as.Date(row.names(total1))
total1$Value <- total1$CP/total1$GDP
total1$percentile <- ecdf(total1$CP/total1$GDP)(total1$CP/total1$GDP)
total1$name <- "Corporate Profits to GDP"
total1$Value2 <- total1$CPATAX/total1$GDP

total2 <- total1
total2$Value <- lm(log(total1$CP)~total1$Date)$residuals
total2$percentile <- ecdf(lm(log(total1$CP)~total1$Date)$residuals)(lm(log(total1$CP)~total1$Date)$residuals)
total2$name <- "Corporate Profits to Regression"

total1 <- na.omit(total1)
ffy1 <- ggplot(total1, aes(y=log(CP), x=Date))+
  geom_line()+
  geom_smooth(method='lm', colour = "black", size = 0.1)+
  ylab("")+
  xlab("")+
  ggtitle(paste("Corporate Profits After Tax", as.Date(total1$Date)[nrow(total1)]))+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

ffy2 <- ggplot(total1, aes(y=lm(log(CP)~Date)$residuals, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = sd(lm(log(CP) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = 2*sd(lm(log(CP) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -1*sd(lm(log(CP) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -2*sd(lm(log(CP) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("Corporate Profits After Tax to Trend")+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(ffy1, ffy2, ncol = 1)
#The two macro factors that have detracted from earnings thus far this year: 
#the strength of the U.S. dollar and dismal energy sector earnings from low oil prices.


ggplot(total1, aes(y=CP/GDP, x=Date))+
  geom_line()+
  geom_line(aes(y=CPATAX/GDP, x=Date, col ="red"))+
  geom_hline(aes(yintercept = mean(CP/GDP, na.rm = T)))+
  geom_hline(aes(yintercept = mean(CP/GDP, na.rm = T)-1*sd(CP/GDP, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(CP/GDP, na.rm = T)+2*sd(CP/GDP, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Corporate Profits After Tax to Real GDP Quarterly", as.Date(total1$Date)[nrow(total1)]))+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

getSymbols(c("MVEONWMVBSNNCB", "CP", "CPATAX"), src = "FRED")
corp_eq_prof <- merge(MVEONWMVBSNNCB, CP, CPATAX)
corp_eq_prof2 <- merge(MVEONWMVBSNNCB, CP, CPATAX)
corp_eq_prof$Value <- MVEONWMVBSNNCB/CP
corp_eq_prof2$Value <- MVEONWMVBSNNCB/CPATAX
corp_eq_prof <- data.frame(corp_eq_prof)
corp_eq_prof2 <- data.frame(corp_eq_prof2)
corp_eq_prof$Date <- as.Date(row.names(corp_eq_prof))
corp_eq_prof2$Date <- as.Date(row.names(corp_eq_prof2))
corp_eq_prof <- na.omit(corp_eq_prof)
corp_eq_prof2 <- na.omit(corp_eq_prof2)

ggplot(corp_eq_prof, aes(x=Date, y = Value))+
  geom_line()+
  geom_line(data=corp_eq_prof2, aes(x=Date, y = Value, col = "red"))+
  ylab("")+
  xlab("")+
  ggtitle(paste("Corporate Equities to Corporate Profits (with/without IVA and CCAdj)", as.Date(corp_eq_prof$Date)[nrow(corp_eq_prof)]))+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))


corp_eq_prof2$percentile <- ecdf(corp_eq_prof2$Value)(corp_eq_prof2$Value)
corp_eq_prof2$name <- "Equities to Profits IVA/CCAdj"
corp_eq_prof2 <- corp_eq_prof2[order(as.Date(corp_eq_prof2$Date), decreasing = TRUE),]

PBV <-  Quandl("MULTPL/SP500_PBV_RATIO_QUARTER", trim_start = "2003-01-01")
PBV <- na.omit(corp_eq_prof)
PBV$percentile <- ecdf(PBV$Value)(PBV$Value)
PBV$name <- "Price to Book Value"

ggplot(PBV, aes(x=Date, y = Value))+
  geom_line()+
  ylab("")+
  xlab("")+
  geom_hline(aes(yintercept = mean(Value, na.rm = T)))+
  geom_hline(aes(yintercept = mean(Value, na.rm = T)-1*sd(Value, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = mean(Value, na.rm = T)+2*sd(Value, na.rm = T)), linetype = "dashed")+
  ggtitle(paste("Price to Book Value", as.Date(PBV$Date)[nrow(PBV)]))+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

PBV <- PBV[order(as.Date(PBV$Date), decreasing = TRUE),]


df <- rbind(Shiller[,c("name","percentile","Value")], 
            Tobin[,c("name","percentile","Value")], 
            SPYmultpl[,c("name","percentile","Value")], 
            SPYGSPC[,c("name","percentile","Value")],
            na.omit(df_1[,c("name","percentile","Value")]),
            na.omit(df_2[,c("name","percentile","Value")]),
            na.omit(total1[,c("name","percentile","Value")]),
            na.omit(total2[,c("name","percentile","Value")]),
            na.omit(corp_eq_prof2[,c("name","percentile","Value")]),
            na.omit(PBV[,c("name","percentile","Value")]))

#Showing US corporate profits with inventory valuation adjustment and capital consumption adjustment - GS
ggplot(df, aes(y=Value,x=percentile))+
  geom_point()+
  facet_wrap(~name, scale = "free")+
  geom_point(data=df[df$name %in% "P/E10",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "Tobins Q",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "SPY from Regression (Nominal)",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "SPY from Regression (Real)",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "Market Capitalization to GDP",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "Corporate Equities to GDP",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "Corporate Profits to GDP",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "Corporate Profits to Regression",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "Equities to Profits IVA/CCAdj",][1,], aes(y=Value,x=percentile), col = "red")+
  geom_point(data=df[df$name %in% "Price to Book Value",][1,], aes(y=Value,x=percentile), col = "red")+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position = "None",
        text = element_text(size=10))

browseURL("https://www.yardeni.com/pub/peacockfeval.pdf") #page 18

#Earnings yields should be above Baa yields
browseURL("http://www.multpl.com/s-p-500-earnings-yield")-browseURL("https://fred.stlouisfed.org/series/BAA")
browseURL("http://www.macrotrends.net/2574/dow-to-gdp-ratio-chart")


#Higher-than-expected rates would obviously depress bond prices, and, in similar fashion, could depress the market's PE ratio (which is the inverse of the earnings yield on equities, and thus similar to a bond price), thus limiting further gains in equity prices to a rate that is somewhat less than the increase in earnings. 
#So even if Trump and the Republicans are successful, we are probably not on the cusp of a monster equity rally. In other words, the next Wall of Worry could be higher-than-expected interest rates, which would put downward pressure on equity prices because they would imply a higher discount rate and a lower present value of future after-tax corporate profits. 
#That downward pressure on equity prices would probably be offset—though not entirely—by rising earnings.

browseURL("http://www.macrotrends.net/1447/ted-spread-historical-chart")

#https://am.jpmorgan.com/us/en/asset-management/gim/adv/insights/guide-to-the-markets/viewer#/top
#page 8: Forward PE 5
#page 6: Lower Forward PE - Better gains in 5 year window
#page 14: All down moves have been characterized by Commodity Spikes, Aggressive Fed or Extreme Valuations. None are present now.
#"Overvalued" on longer-term charts

#Real yields are very low and equity earnings yields are very high for the same reason:  
#Earnings yields: http://www.multpl.com/s-p-500-earnings-yield
#the market is very fearful that a recession and/or a period of prolonged economic stagnation awaits us. 
#The market is unwilling to believe that the record-high level of corporate profits will last. 
#The market is priced to the expectation that the economy will be very weak and profits will collapse.


#At a time when corporate profits have been unusually strong relative to nominal GDP, 
#it is worth noting that investors' valuation of those profits (i.e., how much they are willing to pay per dollar of profits)
#is only about average, according to a variety of measures detailed above. 
#Current valuations thus appear to incorporate a substantial amount of skepticism about future profits. 
#Put another way, there is no evidence here that equity valuations are in "bubble" territory.

#This extension is more than 2 standard deviations from the norm (Jeremy Grantham's definition of a true bubble).
#Buffett himself says one would have to be "wildly optimistic" to expect corporate profits to top 6% for a sustained period.
#HOWEVER, I've been arguing for years that corporate profits relative to GDP are not necessarily going to revert to their
#historical mean of 6.5% because of globalization. Major U.S. companies now earn a substantial portion of their 
#profits overseas, and overseas markets have grown much faster than than the U.S. market (think China and India). 
#When profits are measured relative to global GDP (see chart above) they are only slightly above their long-term average. 


Earningsmultpl <- Quandl("MULTPL/SP500_EARNINGS_MONTH")

sp <- ggplot(Earningsmultpl, aes(y=log(Value), x=Date))+
  geom_line()+
  geom_smooth(method='lm', colour = "black", size = 0.1)+
  ylab("")+
  xlab("")+
  ggtitle(paste("S&P500 Logarithmic Earnings", as.Date(Earningsmultpl$Date)[1]))+
  scale_x_date(date_breaks = "20 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

sp1 <- ggplot(Earningsmultpl , aes(y=lm(log(Value) ~ Date)$residuals, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  #geom_hline(aes(yintercept = sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = 2*sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  #geom_hline(aes(yintercept = -1*sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -2*sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("S&P500 Logarithmic Earnings to Trend")+
  scale_x_date(date_breaks = "20 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(sp, sp1)

Dividendsmultpl <- Quandl("MULTPL/SP500_DIV_MONTH")

sp <- ggplot(Dividendsmultpl , aes(y=log(Value), x=Date))+
  geom_line()+
  geom_smooth(method='lm', colour = "black", size = 0.1)+
  ylab("")+
  xlab("")+
  ggtitle(paste("S&P500 Logarithmic Dividends", as.Date(Dividendsmultpl$Date)[1]))+
  scale_x_date(date_breaks = "20 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

sp1 <- ggplot(Dividendsmultpl , aes(y=lm(log(Value) ~ Date)$residuals, x=Date))+
  geom_line()+
  geom_hline(yintercept = 0)+
  #geom_hline(aes(yintercept = sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = 2*sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  #geom_hline(aes(yintercept = -1*sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  geom_hline(aes(yintercept = -2*sd(lm(log(Value) ~ Date)$residuals, na.rm = T)), linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle("S&P500 Logarithmic Dividends to Trend")+
  scale_x_date(date_breaks = "20 years", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(sp, sp1)
#Here we see the dividends and earnings for SP500. 
#Both are high and markets don't expect them to stay this high when looking at real tips yields and PEs. Revision to the mean.


#The PE ratio of the S&P 500 has dropped to its long-term average of 16.5. So stocks are at "average" valuations today. 
#But: corporate profits are still very high relative to GDP (9-10% of GDP, versus a long-term average of 6-7%). 
#The PE ratio on 10-yr Treasuries is about 50 today (the inverse of their 2% yield). 
#What this means is that the market is deeply distrustful of the staying power of corporate profits. Deeply. 
#Investors are willing to accept extremely low yields on safe assets in order to avoid the perceived risk of stocks. 
#There is a LOT of fear priced into the market, even though PE ratios are not historically depressed.

#Recession Indicators
getSymbols("CFNAIMA3", src = "FRED")
USCOIN <- Quandl("ECRI/USCOIN", trim_start = "1965-01-01")
ECRI <- Quandl("ECRI/USLEADING")
CFNAIMA3 <- data.frame(CFNAIMA3)
CFNAIMA3$Date <- as.Date(row.names(CFNAIMA3))

USCF <- merge(CFNAIMA3, USCOIN, by = "Date")
USCF <- USCF[USCF$Date >= "2000-01-01",]
ECRI <- ECRI[ECRI$Date >= "2000-01-01",]

getSymbols("^GSPC", src = "yahoo", from = "2000-01-01")
GSPC_df <- data.frame(to.weekly(GSPC))
GSPC_df$Date <- as.Date(row.names(GSPC_df))

ECRI <- merge(ECRI, GSPC_df)

aa <- ggplot(USCF, aes(x=Date, y=CFNAIMA3))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = -0.7, linetype = "dashed")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Chicago Fed's National Activity Index Monthly", USCF$Date[nrow(USCF)]))+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

bb <- ggplot(USCF, aes(x=Date, y=Growth))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("ECRI Coincident Index Monthly")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

cc <- ggplot(ECRI, aes(x=Date, y=scale(Growth)))+
  geom_line()+
  geom_line(aes(x = Date, y = scale(Delt(GSPC.Adjusted, k = (52/2)*1))), col = "red")+
  ylab("")+
  xlab("")+
  geom_hline(yintercept = 0)+
  ggtitle("ECRI Leading Index Weekly")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(aa,bb,cc,ncol = 1)
#When the Chicago Fed's line moves below -0.7 following a period of economic expansion, there is an
#increasing likelihood that a recession has begun. Conversly, when the line moves above -0.7
#following a period of economic contraction, there is an increasing likelihood that a recession has ended.

ggplot(ECRI[ECRI$Date >= "2000-01-01",], aes(x=Date, y=scale(Level)))+
  geom_line()+
  geom_line(aes(x = Date, y = scale(GSPC.Adjusted)), col = "red")+
  ylab("")+
  xlab("")+
  ggtitle("ECRI Leading Index")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
#The ECRI's weekly leading index - a weighted composite of 7 economic and market measures including unemployment claims,
#corporate bond yields, industrial material prices, real estate loans, business failures and real money supply - bottomed in 2009 and is now at a new high. 
#Macro fundamentals have traced the same pattern as equity prices, even dipping together in 2010, 2011 and 2015-16
#This positive trend in the macro environment has, unsurprisingly, led to a rise in corporate earnings, which also have a long record of being highly correlated with equity prices. 

#Oil daily: FRED/DCOILBRENTEU
#Gold daily: WGC/GOLD_DAILY_USD


misery <- Quandl("USMISERY/INDEX", trim_start = "1995-01-01")
names(misery) <- c("date","unemp","inf","mis")
ggplot(misery, aes(y=mis,x=date))+
  geom_line()+
  geom_line(aes(y=inf, x=date), col = "red", alpha = 0.4, linetype = "dashed")+
  geom_line(aes(y=unemp, x=date), col = "blue", alpha = 0.4, linetype = "dashed")+
  xlab("")+
  ylab("")+
  ggtitle(paste("Misery Index as of", as.Date(misery$date)[1]))+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text())
#The theory is that less misery should justify a higher P/E. 
#A low unemployment rate should be bullish for stocks unless it is accompanied by rising inflation, 
#which could cause the Fed to tighten to the point of triggering a recession and driving the jobless rate higher. 
#Nirvana should be a low unemployment rate with low inflation, which seems to be the current situation. 
#In this happy state, a recession is nowhere to be seen, which should justify a higher valuation multiple. 

#Having slow economic growth with subdued inflation and low interest rates increases the odds of a very long economic expansion, 
#with the labor market continuing to tighten.
#That would be ideal for my “long good buy” scenario for the stock market, 
#since bull markets usually don’t end until the unemployment rate falls to its cyclical trough and 
#starts moving higher. The stock market also does well when the Misery Index, 
#which is the sum of the unemployment rate and the inflation rate, is falling. 
#Indeed, there is an inverse correlation between the Misery Index and the S&P 500 P/E since 1979. 

Dividendsmultpl <- Quandl("MULTPL/SP500_DIV_YIELD_MONTH", order = "desc")
FRED2 <- Quandl("FED/SVENPY", collapse = "monthly", order = "desc")
#SPYmultpl <- Quandl("MULTPL/SP500_INFLADJ_MONTH", order = "desc")
#Eyield <- Quandl("MULTPL/SP500_EARNINGS_YIELD_MONTH", order = "desc")

tt <- merge(Dividendsmultpl, FRED2, by = "Date")

#tt <- merge(tt, SPYmultpl, by = "Date")
#tt <- merge(tt, Eyield, by = "Date")

tt$c <- tt$SVENPY10-tt$Value
ggplot(tt[tt$Date >= "1990-01-01",], aes(x=Date,y=c))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "longdash")+
  ylab("")+
  xlab("")+
  ggtitle(paste("10 year yield - S&P dividend yield as of", as.Date(tt$Date)[nrow(tt)]))+
  scale_x_date(date_breaks = "3 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
#10 year yield - s&p dividend yield is once again approach approaching 0. 
#when the ratio is negative, it is only natural that money moves out of treasuries into equities in search of yield. 
#TigerTrade: http://www.multpl.com/s-p-500-dividend-yield/ and http://www.multpl.com/10-year-treasury-rate
#If positive money flows into stocks
#s&p dividend yield - 10 year yield is once again approach approaching 0. when the ratio is positive, 
#it is only natural that money moves out of treasuries into equities in search of yield. 
#Positive ratios preceded the 2009 market bottom and the 2012 rally in the spx (1.97 - 1.92 = 0.05). 
#Eventually, the positive ratio will push money to equity, but it doesn't have to happen immediately. 
#There can be quite a lag, but it is something to watch for.


getSymbols("FRBLMCI", src = "FRED")
FRBLMCI <- data.frame(FRBLMCI)
FRBLMCI$Date <- as.Date(row.names(FRBLMCI))
FRBLMCI$Cumsum <- cumsum(FRBLMCI$FRBLMCI)

FRBL <- ggplot(FRBLMCI, aes(x=Date, y=FRBLMCI))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  ggtitle("Labor Market Conditions Index")+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

FRBLC <- ggplot(FRBLMCI, aes(x=Date, y=Cumsum))+
  geom_line()+
  ylab("")+
  xlab("")+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = 2*sd(Cumsum)), linetype = "dashed")+
  geom_hline(aes(yintercept = -1*sd(Cumsum)), linetype = "dashed")+
  ggtitle(paste("Labor Market Conditions Index Cumulative", FRBLMCI$Date[nrow(FRBLMCI)]))+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(FRBLC, FRBL,ncol = 1)
#The indicator, designed to illustrate expansion and contraction of labor market conditions.
#Derived from a dynamic factor model.

#S&P500
#BVPS <-  Quandl("MULTPL/SP500_BVPS_QUARTER", trim_start = "2003-01-01")#S&P 500 book value per share non-inflation adjusted current dollars.
#PSR <-  Quandl("MULTPL/SP500_PSR_QUARTER", trim_start = "2003-01-01")#S&P 500 Price to Sales Ratio (P/S or Price to Revenue). Current price to sales ratio is estimated based on current market price and 12 month sales 
#RS <-  Quandl("MULTPL/SP500_REAL_SALES_QUARTER", trim_start = "2003-01-01")#Trailing twelve month S&P 500 Sales Per Share (S&P 500 Revenue Per Share) inflation-adjusted. yoy correlates with ISM
#RSG <-  Quandl("MULTPL/SP500_REAL_SALES_GROWTH_QUARTER", trim_start = "2003-01-01")#S&P 500 real sales growth rate per year. Annual percentage change in 12 month S&P 500 Real Sales (inflation adjusted).

Real_RS <- Quandl("MULTPL/SP500_REAL_SALES_QUARTER")
Nom_RS <- Quandl("MULTPL/SP500_SALES_QUARTER")

df <- merge(Real_RS, Nom_RS, by = "Date")

a <- ggplot(df, aes(x=Date, y=Value.x))+
  geom_line(col = "darkgreen")+
  geom_line(aes(x=Date, y=Value.y), col = "darkblue")+
  ylab("")+
  xlab("")+
  ggtitle(paste("Real and Nominal S&P500 Sales per Share", df$Date[nrow(df)]))+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

b <- ggplot(df, aes(x=Date, y=Value.x))+
  geom_line(aes(x=Date, y=Delt(Value.x, k=4)), col = "darkgreen")+
  geom_line(aes(x=Date, y=Delt(Value.y, k=4)), col = "darkblue")+
  ylab("")+
  xlab("")+
  geom_hline(yintercept=0)+
  ggtitle("Real and Nominal S&P500 Sales per Share yoy growth")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(a,b)

browseURL("https://www.yardeni.com/pub/peacocksp500.pdf")

#Trailing twelve month S&P 500 Sales Per Share (S&P 500 Revenue Per Share) inflation-adjusted. 
#yoy correlates with ISM

SPCOMP <- Quandl("YALE/SPCOMP", order = "asc")
names(SPCOMP)[grep("Real Price", names(SPCOMP))] <- "Real"
names(SPCOMP)[grep("S&P Composite", names(SPCOMP))] <- "Price"
SPCOMP$real_ret <- Delt(SPCOMP[,"Real"], k=(12*5))
SPCOMP$ret <- Delt(SPCOMP[,"Real"], k=(12*2))

SPCOMP$real_ret_mean <- mean(SPCOMP$real_ret, na.rm = T)
SPCOMP$real_ret_sd <- sd(SPCOMP$real_ret, na.rm = T)
SPCOMP$ind <- ifelse(SPCOMP$real_ret <= SPCOMP$real_ret_mean+2*SPCOMP$real_ret_sd, "up", "down")
SPCOMP$ret_mean <- mean(SPCOMP$ret, na.rm = T)
SPCOMP$ret_sd <- sd(SPCOMP$ret, na.rm = T)
SPCOMP$ind_2 <- ifelse(SPCOMP$ret <= SPCOMP$ret_mean+2*SPCOMP$ret_sd, "up", "down")

real <- ggplot(SPCOMP[SPCOMP$Year >= "1950-01-01",], aes(x=Year, y=real_ret))+
  geom_line()+
  geom_line(aes(x=Year, y=log(Real)-4))+
  geom_point(aes(x=Year, y=log(Real)-4, col = ind), size = 0.1)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = real_ret_mean), linetype = "longdash", alpha = 0.1)+
  ggtitle("Real Returns % Change from 5 Year Ago")+
  ylab("")+
  xlab("")+
  geom_hline(aes(yintercept = real_ret_mean+2*real_ret_sd), linetype = "longdash", col = "red", alpha = 0.2)+
  geom_hline(aes(yintercept = real_ret_mean+3*real_ret_sd), linetype = "longdash", col = "red", alpha = 0.1)+
  scale_x_date(date_breaks = "10 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

nom <- ggplot(SPCOMP[SPCOMP$Year >= "1950-01-01",], aes(x=Year, y=ret))+
  geom_line()+
  geom_line(aes(x=Year, y=log(Price)-2))+
  geom_point(aes(x=Year, y=log(Price)-2, col = ind_2), size = 0.1)+
  geom_hline(yintercept = 0)+
  geom_hline(aes(yintercept = ret_mean), linetype = "longdash", alpha = 0.1)+
  ggtitle("Real Returns % Change from 2 Year Ago")+
  ylab("")+
  xlab("")+
  geom_hline(aes(yintercept = ret_mean+2*ret_sd), linetype = "longdash", col = "red", alpha = 0.2)+
  geom_hline(aes(yintercept = ret_mean+3*ret_sd), linetype = "longdash", col = "red", alpha = 0.1)+
  scale_x_date(date_breaks = "10 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(real, nom) # Do this for earnings


#Gundlach: Under 2 yrs ago the BB Euro junk yielded approx 400 bp above 5 yr UST. 
#And don’t even look at ‘11 or, God forbid, the credit crisis. Beware!
#The BB-rated slice of the Eurozone junk bond index yields less that the 5 year UST.  
#Probably the first, & last, time this will ever happen.
getSymbols(c("BAMLHE00EHYIEY","DGS10","DGS5"), src = "FRED")
xt <- merge(BAMLHE00EHYIEY, DGS10)
GUND <- data.frame(xt)
GUND$Date <- as.Date(row.names(GUND))
GUND$Spread <- GUND$BAMLHE00EHYIEY/GUND$DGS10

limited <- ggplot(na.omit(GUND[GUND$Date >="2014-01-01",]), aes(x=Date, y=Spread))+
  geom_line()+
  geom_line(aes(x=Date, y=BAMLHE00EHYIEY), alpha = 0.4, col = "blue")+
  geom_line(aes(x=Date, y=DGS10), alpha = 0.4, col = "red")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  ggtitle(paste("Euro Junk-Bonds vs 10Y UST", GUND$Date[nrow(GUND)]))+
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

not_limited <- ggplot(na.omit(GUND[GUND$Date >= "2005-01-01",]), aes(x=Date, y=Spread))+
  geom_line()+
  geom_line(aes(x=Date, y=BAMLHE00EHYIEY), alpha = 0.4, col = "blue")+
  geom_line(aes(x=Date, y=DGS10), alpha = 0.4, col = "red")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(limited, not_limited)

xt <- merge(BAMLHE00EHYIEY, DGS5)
GUND <- data.frame(xt)
GUND$Date <- as.Date(row.names(GUND))
GUND$Spread <- GUND$BAMLHE00EHYIEY/GUND$DGS5

limited5 <- ggplot(na.omit(GUND[GUND$Date >= "2014-01-01",]), aes(x=Date, y=Spread))+
  geom_line()+
  geom_line(aes(x=Date, y=BAMLHE00EHYIEY), alpha = 0.4, col = "blue")+
  geom_line(aes(x=Date, y=DGS5), alpha = 0.4, col = "red")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  ggtitle(paste("Euro Junk-Bonds vs 5Y UST", GUND$Date[nrow(GUND)]))+
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

not_limited5 <- ggplot(na.omit(GUND[GUND$Date >= "2005-01-01",]), aes(x=Date, y=Spread))+
  geom_line()+
  geom_line(aes(x=Date, y=BAMLHE00EHYIEY), alpha = 0.4, col = "blue")+
  geom_line(aes(x=Date, y=DGS5), alpha = 0.4, col = "red")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(limited5, not_limited5)

#Gundlach called it “crazy” that European junk bonds have the same yield as a U.S. Treasury basket (the Merrill Lynch U.S. Treasury Index). 
#He said that spread is typically 700 basis points or more.

#The next financial crisis will not look anything like the last financial crisis did. 
#But it will rhyme. This next chart depicts an extreme example of what is happening around the world. 
#Scary levels of junk-bond debt with covenant-lite options – coupled with the Frank Dodd rules that don’t allow banks to operate in the corporate bond market as market makers – are going to mean that corporate debt, 
#from the worst right on up to the best, will take a massive yield hit, as the flight for cash rhymes with what we saw in 2009.
#Remember, in a crisis you don’t sell what you want to sell; you sell what you can sell. 
#And at a bargain-basement price. 
#We have monster mutual funds and ETFs investing in these high-yield corporate markets, and the redemptions from them are going to force selling into a market where there are no buyers. 
#If you’re wondering what will push the country into recession, look to the financial markets. That’s where the excesses are being created. 


getSymbols("DGS10", src = "FRED", from = "1950-01-01")
getSymbols("^GSPC", src = "yahoo", from = "1950-01-01")

data <- merge(GSPC, DGS10)
data <- na.omit(data)
z <- data[,c("GSPC.Adjusted", "DGS10")]

z.logrtn <- diff(log(z))
c <- cor(z.logrtn,use="complete.obs")
ut <- upper.tri(c)
n <- paste(rownames(c)[row(c)[ut]],rownames(c)[col(c)[ut]])
width <- 365
rollingcorr.1m <- rollapply(z.logrtn,
                            width=width,
                            FUN = function(Z)
                            {
                              return(cor(Z,use="pairwise.complete.obs")[ut])
                            },
                            by.column=FALSE, align="right")
colnames(rollingcorr.1m) <- n
rollingcorr.1m.df <- data.frame(rollingcorr.1m)
#rollingcorr.1m.df$Mean_all <- rowMeans(rollingcorr.1m.df, na.rm = T)
rollingcorr.1m.df$Date <- as.Date(row.names(rollingcorr.1m.df))
df <- data.frame(data)

ggplot(data = rollingcorr.1m.df, aes(x = Date, y = GSPC.Adjusted.DGS10)) +
  geom_line()+
  geom_line(data=data, aes(y=DGS10/10, x = as.Date(row.names(df))), alpha = 0.5)+
  geom_hline(yintercept = 0, linetype = "longdash")+
  #geom_hline(yintercept = 0.3, alpha = 0.5)+
  geom_hline(yintercept = 0.5, alpha = 0.3, linetype = "dashed")+
  ggtitle(paste("Relation Between Yields and Equities", rollingcorr.1m.df$Date[nrow(rollingcorr.1m.df)]))+ 
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "10 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        text = element_text(size=10),
        legend.position = "none")

#High rate-equity correlation is a good indicator of a risk on/off macro environment. 
#This correlation measures the extent to which investors make top-down decisions (e.g., stocks vs. bonds) as opposed views on individual sectors or securities.
#Historically, the correlation between equities and rates was not always positive. 
#Moreover, there are theoretical reasons why this correlation should in fact be negative. 
#The so-called “Fed Model” states that treasury yields should be roughly equal to equity earnings yield (E/P or simply the inverse of the P/E ratio). 
#We note that prior to 1997, rate/equity correlation was indeed negative, as predicted by the so-called “Fed Model.”


getSymbols(c("GDPC1","PAYEMS"), src = "FRED")

PAYEMS <- to.quarterly(PAYEMS)
GDP <- GDPC1

dat <- merge(GDP, PAYEMS[,"PAYEMS.Close"])
dat <- data.frame(dat)
dat <- na.omit(dat)

dat$prod <- Delt(dat$GDP/dat$PAYEMS.Close, k=4*5)
dat$ma <- apply(dat[,"prod"], 2, SMA, n=4)

ggplot(dat, aes(y=prod, x=as.Date(row.names(dat))))+
  geom_line()+
  geom_line(aes(y=ma, x=as.Date(row.names(dat))), col = "red", alpha = 0.3) +
  ggtitle(paste("Productivity Growth", as.Date(row.names(dat))[nrow(dat)]))+
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "10 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
#The chart takes the ratio of gross domestic product / total employees and annualized a 
#productivity growth rate to see if the current jobs we are adding are getting more or less productive, an insight into quality.
#That is a devastating sign for the current economy, a large reason for the lackluster 
#GDP growth year in and year out, and also why interest rates are continuing to move lower.
#It truly should come as no surprise that crashing productivity would translate to poor wage growth.

#The best measure of wage growth in the economy is 'Total Aggregate Wages' measured as:
#Total Employees * Average Hourly Earnings * Average Hours Worked
#This measure shows the total dollars earned in the economy, and putting this number into a growth rate 
#shows whether more or less dollars were earned/circulating around the economy.
getSymbols(c("CES0500000003","PAYEMS", "AWHAETP", "CUUR0000SEHA","PCEPILFE"), src = "FRED")

data <- merge(CES0500000003,PAYEMS,AWHAETP,CUUR0000SEHA,PCEPILFE)
data <- data.frame(data)
#data <- na.omit(data)
data$Agg <- data$CES0500000003*data$AWHAETP*data$PAYEMS
data$Date <- as.Date(row.names(data))
data$Agg_1 <- Delt(data$Agg, k = 12)
data$inf <- Delt(data$CUUR0000SEHA, k = 12)
data$Agg_1_less <- data$Agg_1-data$inf
data$pce <- Delt(data$PCEPILFE, k = 12)
data$Agg_1_less_pce <- data$Agg_1-data$pce

ggplot(data[data$Date >= "2007-01-01",], aes(y=Agg_1, x=Date))+
  geom_line()+
  geom_line(aes(y=Agg_1_less, x=Date), col = "red", alpha = 0.3) +
  geom_line(aes(y=Agg_1_less_pce, x=Date), col = "purple", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  ggtitle(paste("Aggregate Earnings Growth/Shelter Inflation Corrected/PCE Corrected", data$Date[nrow(data)]))+
  ylab("")+
  xlab("")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
#https://seekingalpha.com/article/4105962-interest-rates-will-rise
#subtracts the rate of rent inflation to show how consumer wage growth is doing relative to increases in the cost of living, namely shelter.
#Interest rates will not rise unless growth rises. Growth will not rise unless wage growth rises. Wage growth will not rise unless productivity rises.


#Fear and Greed
browseURL("http://money.cnn.com/data/fear-and-greed/")
#Make this for yourself!

#Citibank Surprise index
browseURL("https://www.yardeni.com/pub/citigroup.pdf")
#A positive reading of the Economic Surprise Index suggests that economic releases have on balance been beating consensus. 

#Flow of Funds:
browseURL("https://whalewisdom.com/filer/soros-fund-management-llc")
browseURL("https://whalewisdom.com/filer/bridgewater-associates-inc")
browseURL("https://whalewisdom.com/filer/hussman-econometrics-advisors-inc")
browseURL("http://www.etf.com/etfanalytics/etf-fund-flows-tool")

#Gundlach
browseURL("https://doubleline.com/category/commentary/")
browseURL("https://twitter.com/truthgundlach?lang=fi")
browseURL("https://search.cnbc.com/rs/search/view.html?source=CNBC.com&categories=exclude&partnerId=2000&keywords=JEFF%20GUNDLACH")
browseURL("https://www.bloomberg.com/search?query=Jeff+Gundlach")
browseURL("https://doublelinefunds.com/webcasts/")

#Hussman
browseURL("http://www.hussman.com/weeklyMarketComment.html")
browseURL("http://www.hussman.com/researchInsight.html")
browseURL("https://twitter.com/hussmanjp?lang=en")

#Dalio
browseURL("https://twitter.com/raydalio?lang=en")
browseURL("https://search.cnbc.com/rs/search/view.html?source=CNBC.com&categories=exclude&partnerId=2000&keywords=RAY%20DALIO")
browseURL("https://www.bloomberg.com/search?query=Ray+Dalio")

#Urban Carmel
browseURL("https://twitter.com/ukarlewitz?lang=fi")

#gapcap1
browseURL("https://imgur.com/a/qJpSZ")

browseURL("https://twitter.com/calculatedrisk")


#In summary, interest rates are mostly a function of growth and inflation, both of which remain modest. 
#Unless these unexpectedly begin to accelerate, it is likely that the QT program will not, by itself, usher in a period of much higher rates. 
#In any case, equity prices and valuations are not strongly inversely correlated to rates: they often rise together as faster growth makes companies more valuable.


#replace MULTPL/ with SPCOMP data
#real prices too!
#Quarterly vwap and mad

#correlations and regressions on: (cross-asset vol)
getSymbols("^VVIX", from = "1990-01-01")

#If up by 4% in jan where do we close at by year end

#High to low days without corrections

