from splinter import Browser
import json

browser = Browser()
browser.visit('https://netnutrition.dining.cornell.edu/NetNutrition/1')

item_ingredient_list = {}


def wait(tag):
    browser.is_element_not_present_by_tag(tag, wait_time=1)


for e in range(len(browser.find_by_tag('a[class="cbo_nn_unitNameLink unit__name-link "]'))):
    wait('a[class="cbo_nn_unitNameLink unit__name-link "]')
    eatery = browser.find_by_tag(
        'a[class="cbo_nn_unitNameLink unit__name-link "]')[e]
    eatery.click()

    for m in range(len(browser.find_by_tag('a[class="cbo_nn_menuLink"]'))):
        wait('a[class="cbo_nn_menuLink"]')
        menu = browser.find_by_tag('a[class="cbo_nn_menuLink"]')[m]
        menu.click()

        for i in range(len(browser.find_by_tag('a[class="cbo_nn_itemHover"]'))):
            wait('a[class="cbo_nn_itemHover"]')
            item = browser.find_by_tag('a[class="cbo_nn_itemHover"]')[i]
            item.click()

            wait('td[class="cbo_nn_LabelHeader"]')
            item_name = browser.find_by_tag(
                'td[class="cbo_nn_LabelHeader"]').value

            try:
                ingredients_label = browser.find_by_tag(
                    'span[class="cbo_nn_LabelIngredients"]').value
            except:
                ingredients_label = ""

            item_ingredient_list[item_name] = ingredients_label
            wait('btn_nn_nutrition_close')
            browser.find_by_id('btn_nn_nutrition_close').click()
        wait(
            'a[onclick="NetNutrition.UI.menuDetailBackBtn()"]')
        browser.find_by_tag(
            'a[onclick="NetNutrition.UI.menuDetailBackBtn()"]').click()

    wait(
        'a[onclick="NetNutrition.UI.courseListBackBtn(event)"]')
    try:
        browser.find_by_tag(
            'a[onclick="NetNutrition.UI.courseListBackBtn(event)"]').click()
    except:
        browser.find_by_tag(
            'a[onclick="NetNutrition.UI.menuDetailBackBtn()"]').click()


with open('net_nutrition.json', 'w') as json_file:
    json.dump(item_ingredient_list, json_file)

browser.quit()
