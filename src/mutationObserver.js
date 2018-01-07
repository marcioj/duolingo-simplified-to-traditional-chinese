import { Duolingo } from './duolingo.js';
import { Cache } from './cache.js';
import { CharacterLookup } from './characterLookup.js'

let characterLookup = new CharacterLookup();

export const mutationObserver = new MutationObserver(function(mutations){

    if(Duolingo.isLearningChinese() == false){
      return false;
    }

    for (let i=0; i < mutations.length; i++){
      for (let j=0; j < mutations[i].addedNodes.length; j++){
          const node = mutations[i].addedNodes[j]
          const nodeSting = node.textContent;
          const asciiStringArray = nodeSting.replace(/[^\x00-\x7F]/g, "");
          if(nodeSting.match(/[\u3400-\u9FBF]/)){

            let difference = nodeSting.split('').filter(x => asciiStringArray.indexOf(x) == -1);

            if(difference.length == 0 ){
             return;
            }
            const chineseCharacters = [];
            const compoundChineseCharacters = [];
            let lastCharacterChinese = false;
            let lastCharacter = '';
            difference.forEach(character => {

                if( character > "\u3400" && character < "\u9FBF"){
                  chineseCharacters.push(character)
                  lastCharacterChinese = true;

                  if(lastCharacter != '' && lastCharacterChinese == true){
                      compoundChineseCharacters.push(lastCharacter + character );
                  }
                  lastCharacter = character;

                }else{
                  lastCharacterChinese = false;
                  lastCharacter='';
                }

            })

            compoundChineseCharacters.forEach(compoundCharacter=>{
                  const result = characterLookup.getMeaning(compoundCharacter);
                  if(result){
                    chineseCharacters.unshift(compoundCharacter);
                  }
            });


            chineseCharacters.forEach((character) => {
                if (localStorage.getItem(character) !== null){
                    if (localStorage.getItem(character) != undefined){
                      Duolingo.insertCharacter(character, localStorage.getItem(character));
                    }
                    return null;
                }

                const characterEncoded = encodeURI(character)

                let reqListener = (data)  => {
                  if(!mutations[i].addedNodes[j].innerHTML){
                    return;
                  }
                  if(data.currentTarget.responseText){
                      Cache.setCharacter(character, data.currentTarget.responseText)
                  }
                //  Duolingo.insertCharacter(character, data.currentTarget.responseText)
                }
                if(navigator.onLine){
                  let oReq = new XMLHttpRequest();
                  oReq.addEventListener("load", reqListener);
                  oReq.open("GET", "https://lookup.duochinese.space/?character="+characterEncoded);
                  oReq.send();
              }
          });
        }
      }
    }
  });
