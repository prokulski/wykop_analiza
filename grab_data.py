'''
scrapper danych z głównej strony Wykop.pl
'''

import requests
from bs4 import BeautifulSoup as bs
from itertools import chain
import csv

'''
funkcja z bloku dla jednego znaleziska wydziela gromadzone informacje:
- wykopy = liczba wykopów
- autor = nick, który dodał znalezisko
- data = data dodania znaleziska
- tags = tagi opisujące znalezisko
- tytul = tytuł znaleziska
- link = dokąd prowadzi znalezisko?
- opis = opis znaleziska
'''


def grab_wykop_item_data(fitem):
    wykopy = fitem.select("a.ajax span")
    if len(wykopy) != 0:
        wykopy = int(wykopy[0].get_text())
        autor = fitem.select("div.fix-tagline a.affect")[0].get_text()
        data = fitem.select("div.row.elements span.affect time")[0]["datetime"]
        tytul = fitem.select("h2 a")[0].get_text().strip()
        link = fitem.select("div.fix-tagline span.tag.create a.affect")[0]["href"]
        opis = fitem.select("div.description p.text a")[0].get_text().strip()

        tagi = fitem.select("div.fix-tagline a.tag.affect.create")
        tags = []
        for tag in tagi:
            temp_tag = tag.get_text()
            if temp_tag[0] == "#":
                tags.append(temp_tag)

        return wykopy, autor, data, "||".join(tags), tytul, link, opis

    return None


# [] or list all is loaded to memory
urls = [
    "https://www.wykop.pl/strona/{}".format(page)
    for page in range(1, 3000, 1)
]

# x = () is generator, no action taken until next(x) or for is called
# memory efficient

pages = (requests.get(url) for url in urls)
contents = (page.content for page in pages)
soups = (bs(content, 'html.parser') for content in contents)
lists_of_items = (soup.select('li.iC') for soup in soups)  # just li with class iC
all_items_together = chain.from_iterable(lists_of_items)
extracted = (grab_wykop_item_data(item) for item in all_items_together)
filter_out_nones = (item for item in extracted if item is not None)  # Skip Nones

# no action taken to that point


def write_to_csv():
    with open('wykop.csv', 'w+') as destination_file:
        writer = csv.writer(destination_file, delimiter=';', quotechar='"')

        for row in filter_out_nones:
            writer.writerow(row)


def debug_few_elements():
    print(next(filter_out_nones))
    print(next(filter_out_nones))
    print(next(filter_out_nones))
    print(next(filter_out_nones))


def transform_to_list():
    return list(filter_out_nones)  # danger, load whole content to memory, fine for few items


if __name__ == '__main__':
    debug_few_elements()
