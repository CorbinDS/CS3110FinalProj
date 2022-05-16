import splinter
from splinter import Browser
import json

from selenium.webdriver.firefox.options import Options


def wait(browser, tag):
    browser.is_element_not_present_by_tag(tag, wait_time=1)


def update_net_nutrition():
    print("help")
    options = Options()
    options.headless = True

    browser = Browser(options=options)
    browser.visit('https://netnutrition.dining.cornell.edu/NetNutrition/1')

    item_ingredient_list = {}
    item_ingredient_list["eateries"] = []
    #browser.find_by_tag(cbo_nn_unitNameLink unit__name-link) is a list of all the links to menus
    for e in range(len(browser.find_by_tag('a[class="cbo_nn_unitNameLink unit__name-link "]'))):
        wait(browser, 'a[class="cbo_nn_unitNameLink unit__name-link "]')
        eatery = browser.find_by_tag(
            'a[class="cbo_nn_unitNameLink unit__name-link "]')[e]
        eatery_name = browser.find_by_tag(
            'a[class="cbo_nn_unitNameLink unit__name-link "]')[e].value
        eatery_dict = {}
        eatery_dict["name"] = eatery_name
        eatery_dict["menus"] = []
        eatery.click()
        
        #browser.find_by_tag('a[class="cbo_nn_menuLink"]) is a list of all the menu categories in the current eatery
        for m in range(len(browser.find_by_tag('a[class="cbo_nn_menuLink"]'))):
            menu_dict = {}
            wait(browser, 'a[class="cbo_nn_menuLink"]')
            menu = browser.find_by_tag('a[class="cbo_nn_menuLink"]')[m]
            menu_name = browser.find_by_tag('a[class="cbo_nn_menuLink"]')[m].value
            menu_dict["name"] = menu_name
            menu_dict["items"] = [] 
            menu.click()


            #browser.find_by_tag('a[class="cbo_nn_itemHover"]) is a list of all the items in the menu category
            for i in range(len(browser.find_by_tag('a[class="cbo_nn_itemHover"]'))):
                item_dict = {}
                wait(browser, 'a[class="cbo_nn_itemHover"]')
                item = browser.find_by_tag('a[class="cbo_nn_itemHover"]')[i]
                item_name = browser.find_by_tag('a[class="cbo_nn_itemHover"]')[i].value
                item.click()

                wait(browser, 'td[class="cbo_nn_LabelHeader"]')
                item_name = browser.find_by_tag(
                    'td[class="cbo_nn_LabelHeader"]').value

                try:
                    ingredients_label = browser.find_by_tag(
                        'span[class="cbo_nn_LabelIngredients"]').value
                except:
                    ingredients_label = ""

                item_dict[item_name] = ingredients_label
                menu_dict["items"].append(item_dict)

                wait(browser, 'btn_nn_nutrition_close')
                browser.find_by_id('btn_nn_nutrition_close').click()
            wait(browser,
                 'a[onclick="NetNutrition.UI.menuDetailBackBtn()"]')
            browser.find_by_tag(
                'a[onclick="NetNutrition.UI.menuDetailBackBtn()"]').click()

            eatery_dict["menus"].append(menu_dict)

            print("movin along")

        wait(browser,
             'a[onclick="NetNutrition.UI.courseListBackBtn(event)"]')
        try:
            browser.find_by_tag(
                'a[onclick="NetNutrition.UI.courseListBackBtn(event)"]').click()
        except:
            browser.find_by_tag(
                'a[onclick="NetNutrition.UI.menuDetailBackBtn()"]').click()
        
        item_ingredient_list["eateries"].append(eatery_dict)

    with open('database/net_nutrition.json', 'w') as json_file:
        json.dump(item_ingredient_list, json_file)

    browser.quit()


update_net_nutrition()