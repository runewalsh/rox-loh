# Хо-хо! Протоплазма!
Это должна быть игра по мотивам [«Абсолютного оружия»](http://rufina.narod.ru/Shekly/weapon.html) Шекли, но я, наверное, не буду доделывать.  
Не в этом виде.  
Не в этой жизни.  
И уж точно не на легаси-OpenGL.  

Почему вообще легаси? Я подумал, что так будет надёжней. А потом не удержался, заюзал `TEXTURE_SWIZZLE` и наткнулся на [`ARB_texture_swizzle issue 7`](https://www.opengl.org/registry/specs/ARB/texture_swizzle.txt). Вынужден признать: баги шейдеров — меньшее зло по сравнению со взаимодействием легаси и пока-не-легаси.