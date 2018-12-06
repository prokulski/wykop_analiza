import requests
from bs4 import BeautifulSoup
import pandas as pd


def grab_wykop_item_data(fitem):
	wykopy = fitem.select("a.ajax span")
	if(len(wykopy) != 0):
		wykopy = int(wykopy[0].get_text())
		autor = fitem.select("div.fix-tagline a.affect")[0].get_text()
		data = fitem.select("div.row.elements span.affect time")[0]["datetime"]
		tytul =  fitem.select("h2 a")[0].get_text().strip()
		link = fitem.select("div.fix-tagline span.tag.create a.affect")[0]["href"]
		opis = fitem.select("div.description p.text a")[0].get_text().strip()

		tagi = fitem.select("div.fix-tagline a.tag.affect.create")
		tags = []
		for tag in tagi:
			temp_tag = tag.get_text()
			if temp_tag[0] == "#":
				tags.append(temp_tag)

		return True, wykopy, autor, data, tags, tytul, link, opis

	return False, 


dane = pd.DataFrame()

for i in range(1, 3000, 1):
	# budujemy urla
	page_url = f"https://www.wykop.pl/strona/{i}/"
	print(f"{page_url}")

	# wczytujemy kolejną stronę
	page = requests.get(page_url)

	# parser strony
	soup = BeautifulSoup(page.content, 'html.parser')

	# znajdujemy wszystkie elemeny "li"
	items = soup.find_all(("li", ["link", "iC"]))

	# dla każdego z nich
	for item in items:
		# jaką ma klasę?
		try:
			clazz = item["class"]
		except KeyError:
			clazz = ""

		# jeśli jest iC wśród klas to zbieramy to co w środku
		if "iC" in clazz:
			dane_temp = grab_wykop_item_data(item)

			if dane_temp[0]:
				dane_temp = {'wykopy': dane_temp[1],
							'autor': dane_temp[2],
							'data': dane_temp[3],
							'tags': dane_temp[4],
							'tytul': dane_temp[5],
							'link': dane_temp[6],
							'opis': dane_temp[7]
							}
				dane_temp = pd.DataFrame(dane_temp)
				dane_temp.to_csv("plik.csv", mode='a', header = False)
#				dane = dane.append(dane_temp, ignore_index = True)


#dane.to_csv("plik.csv")
#dane.to_json("plik.json")
