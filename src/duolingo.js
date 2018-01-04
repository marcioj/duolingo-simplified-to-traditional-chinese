import { characters } from './characters.js';
import { CharacterLookup } from './characterLookup.js'
let characterLookup = new CharacterLookup();
export class Duolingo{

   static isLearningChinese(){
    if(location.pathname.indexOf("zs/") == -1){
      return false;
    }
    return true;
  }
  static insertCharacter(maoCharacacter, realCharacter){
    let character = realCharacter;
    setTimeout(function(){
          let singleChalengeElement = (document.querySelector('[data-test="challenge-header"] + span > div > div'))
          if(singleChalengeElement !==  null){
            if(singleChalengeElement.childElementCount == 2){
                  const meaning = characterLookup.getMeaning(character);
                  if( meaning !== undefined){
                      let div = document.createElement('div')
                      div.innerHTML=`<div class="meaning"><div class="english">${meaning.meaning}</div><div class="type">${meaning.type}</div></div>`;
                      singleChalengeElement.appendChild(div)
                      console.log('inserted')
                  }
            }
          }else{

          }
    }, 400);
    if(maoCharacacter == realCharacter){
      return;
    }
    if(maoCharacacter == undefined || maoCharacacter == "undefined"){
      return;
    }

    const element = Sizzle(`:contains(${maoCharacacter})`).slice(-1)[0];

    if (element){
      element.innerHTML = element.innerHTML.replace(maoCharacacter,realCharacter)
    }


  }

  static checkForChineseCharactersOnLoad(){
    if(Duolingo.isLearningChinese() == false){
      return;
    }
    characters.forEach((x, y) =>{
      let [simplified, traditional] = x.split('|');
      setTimeout(function(){
          Duolingo.insertCharacter(simplified,traditional);
      },1);
    });

    console.log('initial load');
  }
}
