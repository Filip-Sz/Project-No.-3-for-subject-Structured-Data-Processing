import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


def create_tags_monthly(path, year_start, fileprefix, year_stop=2021, howmany=0):
    '''
    Ta funkcja tworzy i zapisuje ramke danych zawierajaca informacje ile razy w danych miesiacach, w danych latach, wykorzystany byl kazdy tag.
    To wszystko robi na postawie ramki danych Posts z rozwazanego forum stackexchange.

    :param path: str, sciezka do ramki danych Posts w formacie csv
    :param year_start: int, minimalny rok, od ktorego informacje ma zawierac wynikowa ramka danych
    :param fileprefix: str, ciag znakow ktory znajdzie sie w nazwie zapisanego pliku
    :param year_stop: int, maksymalny rok, do ktorego informacje ma zawierac wynikowa ramka danych
    :param howmany: int, opcjonalna zmienna, gdy rozna od 0, funkcja w wynikowej ramce danych umiesci jedynie tyle najpopularniejszych
    tagow z kazdego miesiaca
    :return: pandas.DataFrame, ramka danych zawierajaca informacje ile razy w danych miesiacach, w danych latach, wykorzystany byl kazdy tag (rowniez zapisana jako plik)
    '''

    def moje_replace(tags):
        tags = str(tags)
        return tags.replace("><", ",").replace("<", "").replace(">", "")

    Posts = pd.read_csv(path)

    Posts["Tags"] = Posts["Tags"].apply(moje_replace)
    Posts = Posts.sort_values(by=["CreationDate"])

    Posts["Year"] = pd.DatetimeIndex(Posts["CreationDate"]).year
    Posts["Month"] = pd.DatetimeIndex(Posts["CreationDate"]).month

    Posts = Posts.loc[(Posts["Year"] >= year_start) & (Posts["Year"] <= year_stop)]
    row_num = len(Posts.index)
    result = pd.DataFrame(columns=['Year', 'Month', 'TagName', 'Month_Count'])
    i = 0
    month = 0
    count_dict = {}

    for n, x in enumerate(Posts.iterrows()):
        progress = round((n / row_num) * 100)
        print(f'{progress}%')
        tags = x[1]["Tags"]
        if tags != 'nan':

            if month != x[1]["Month"]:
                this_month_df = pd.DataFrame(columns=['Year', 'Month', 'TagName', 'Month_Count'])
                top_tags = {k: v for k, v in sorted(count_dict.items(), key=lambda item: item[1], reverse=True)}
                if howmany:
                    for count, tagname in enumerate(top_tags):
                        if count == howmany:
                            break
                        this_month_df = this_month_df.append(pd.DataFrame([[year, month, tagname, count_dict[tagname]]],
                                                                          columns=['Year', 'Month', 'TagName',
                                                                                   'Month_Count']))
                else:
                    for tagname in top_tags:
                        this_month_df = this_month_df.append(pd.DataFrame([[year, month, tagname, count_dict[tagname]]],
                                                                          columns=['Year', 'Month', 'TagName',
                                                                                   'Month_Count']))
                result = result.append(this_month_df)
                count_dict = {}
                month = x[1]["Month"]
                year = x[1]["Year"]

            for tag in tags.split(","):
                if tag not in count_dict.keys():
                    count_dict[tag] = 1
                else:
                    count_dict[tag] += 1

    this_month_df = pd.DataFrame(columns=['Year', 'Month', 'TagName', 'Month_Count'])
    top_tags = {k: v for k, v in sorted(count_dict.items(), key=lambda item: item[1], reverse=True)}
    if howmany:
        for count, tagname in enumerate(top_tags):
            if count == howmany:
                break
            this_month_df = this_month_df.append(pd.DataFrame([[year, month, tagname, count_dict[tagname]]],
                                                              columns=['Year', 'Month', 'TagName', 'Month_Count']))
    else:
        for tagname in top_tags:
            this_month_df = this_month_df.append(pd.DataFrame([[year, month, tagname, count_dict[tagname]]],
                                                              columns=['Year', 'Month', 'TagName',
                                                                       'Month_Count']))
    result = result.append(this_month_df)
    result = result.reset_index()
    result = result.drop(columns=['index'])
    if howmany:
        result.to_csv(f"{fileprefix}_Tags_Monthly_Top{howmany}.csv")
    else:
        result.to_csv(f"{fileprefix}_Tags_Monthly_All.csv")
    return result


def monthly_plots(path, year_start, fileprefix, year_stop=2021, top_tags_num=10):
    '''
    Funkcja tworzaca wykresy ilosci najpopularniejszych tagow na przestrzeni roku, wyswietla je i zapisuje

    :param path: str, ciag znakow zwiazany ze sciezka pliku, przy tej implementacji zawsze rowny "Tags_Monthly"
    :param year_start: int, minimalny rok, dla ktorego funkcja ma stworzyc i zapisac wykres
    :param fileprefix: str, ciag znakow ktory znajdzie sie w nazwie zapisanego wykresu, wykorzystywany rowniez do ustaleia sciezki pliku
    :param year_stop: int, maksymalny rok, dla ktorego funkcja ma stworzyc i zapisac wykres
    :param top_tags_num: int, ilosc najpopularniejszych tagow, ktorych popularnosc na przestrzeni roku zostanie przedstawiona na wykresie,
    domyslna wartosc to 10, poniewaz tyle jest domyslnych kolorow na wykresie przez co dla wiekszych wartosci wykresy sa mniej czytelne
    :return: None (funkcja wyswietla i zapisuje wykresy)
    '''
    path_read = fileprefix.capitalize() + '_' + path + '_All.csv'
    path_save = f'Tags_Monthly_Results/{fileprefix}_monthly_plot_'
    Tags_Monthly = pd.read_csv(path_read)
    Tags_Monthly = Tags_Monthly[["Year", "Month", "TagName", "Month_Count"]]
    for year in range(year_start, year_stop + 1):

        Tags_Monthly_Year = Tags_Monthly.loc[Tags_Monthly.Year == year]

        besttags = Tags_Monthly_Year[["TagName", "Month_Count"]].groupby(["TagName"]).sum()
        topbesttags = besttags.sort_values(by=["Month_Count"], ascending=False).head(top_tags_num)
        TagNames = topbesttags.iloc[:, 0].index
        fig = plt.figure()
        ax = fig.add_subplot()
        for tagname in TagNames:
            tag = Tags_Monthly_Year.loc[Tags_Monthly_Year.TagName == tagname]
            ax.plot(tag.Month, tag.Month_Count, label=tagname)

        ax.set_xlabel('Month')
        ax.set_ylabel('Count')
        handles, labels = ax.get_legend_handles_labels()
        lgd = ax.legend(handles, labels, loc='center left', bbox_to_anchor=(1, 0.5))
        text = ax.text(0.37, 1.05, f"{fileprefix.capitalize()} Year {year}", transform=ax.transAxes)
        ax.grid('on')
        fig.savefig(path_save + str(year), bbox_extra_artists=(lgd, text), bbox_inches='tight')
        plt.show()


create_tags_monthly('gaming_csv/Posts.xml.csv', 2016, "Gaming")
create_tags_monthly('sport_csv/Posts.xml.csv', 2016, "Sport")
create_tags_monthly('fitness_csv/Posts.xml.csv', 2016, "Fitness")

monthly_plots("Tags_Monthly", 2016, "gaming", top_tags_num=10)
monthly_plots("Tags_Monthly", 2016, "fitness", top_tags_num=10)
monthly_plots("Tags_Monthly", 2016, "sport", top_tags_num=10)
