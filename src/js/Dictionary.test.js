import test from "ava";
import Dictionary from "./Dictionary";

let dictionary;

test.beforeEach(() => {
  dictionary = new Dictionary();
});

test("translates single character", t => {
  t.is(dictionary.translateToTraditional("梦"), "夢");
});

test("translates compound character", t => {
  t.is(dictionary.translateToTraditional("不客气"), "不客氣");
});
