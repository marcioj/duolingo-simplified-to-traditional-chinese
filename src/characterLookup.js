import { characters } from './characters.js';
export class CharacterLookup{
      constructor(){
        this.dictionary={};
        for(let x in characters){
            let [simplified, traditional, meaning, type] = characters[x].split('|');
            this.dictionary[simplified] = {meaning: meaning, type:type};
            this.dictionary[traditional]  = {meaning:meaning, type: type };
        }
      }

      getMeaning(character){
          return this.dictionary[character];
      }
}
