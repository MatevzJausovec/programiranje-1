# Zbirnik

Pri vseh nalogah se vam pod razdelkom **RAM** splača nastaviti _View_ na _Decimal_, da bodo vrednosti v pomnilniku predstavljene z običajnim desetiškim zapisom. Prav tako lahko pod možnostjo _Register addressing_ obkljukajte tiste registre, v katerih bodo shranjeni naslovi, saj bodo tisti naslovi ustrezno obarvani.

## Računanje ostanka

Zapišite program, ki v register `A` zapiše ostanek pri deljenju registra `A` z registrom `B`.

    Numb 1
    
    MOV A, 27
    MOV B, 10
    MOV C, 232
    
    main:
	    CMP B, A
	    JC odstej
	    MOV [C], A
	    HLT
    
    odstej:
	    SUB A, B
	    JMP main
    
    Numb 2
    
    MOV A, 27
    MOV B, 10
    MOV C, A
    
    DIV B
    MUL B
    SUB C, A
    MOV [232], C

## Zaporedna števila

Zapišite program, ki na sklad zaporedno postavlja števila od 13 do 42.

    MOV A, 13
    MOV B, 42
    
    looper:
    PUSH A
    INC A
    CMP B, A
    JC end
    JMP looper
    
    end:
    HLT

## Iskanje najmanjšega števila v seznamu

Zapišite program, ki poišče najmanjše število v danem seznamu. Seznam naj bo podan na začetku pomnilnika in sicer tako, da je na prvih dveh bajtih ukaz za skok na začetek programa, v tretjem bajtu dolžina seznama, v naslednjih bajtih zaporedoma elementi seznama, takoj za njimi pa bajt, v katerega naj se zapiše najmanjše število.

    JMP main
    dolzina:
        DB 10    ; število elementov v seznamu
    seznam:
        DB 50    ; seznam
        DB 56
        DB 60
        DB 46
        DB 44
        DB 58
        DB 42
        DB 52
        DB 48
        DB 54
    minimum:
        DB 0    ; na koncu bo tu minimum

    main:
        ...
    -----------------------------
    JMP main
    dolzina:
        DB 10    ; število elementov v seznamu
    seznam:
        DB 50    ; seznam
        DB 56
        DB 60
        DB 46
        DB 44
        DB 58
        DB 42
        DB 52
        DB 48
        DB 54
    minimum:
        DB 0    ; na koncu bo tu minimum

    main:
    MOV D, 1
    MOV C, [D+2]
    MOV [minimum], C
    INC D

    looop:
    	MOV C, [D+2]
    	CMP C, [minimum]
    	JC small
    	back:
    	CMP D, [dolzina]
    	JNC end
    	INC D
    	JMP looop
    

    small:
    	MOV [minimum], C
    	JMP back
    end:
    	MOV C, [minimum]
    	MOV [232], C
    	HLT


## Indeks najmanjšega števila v seznamu

Zapišite funkcijo `poisci_minimum`, ki v register `B` shrani indeks najmanjšega števila v rezini `[A:C]` torej med števili, ki se nahajajo od vključno naslova, shranjenega v `A`, do naslova pred tistim, shranjenim v `C`. Funkcija naj z izjemo registra `B` vrednosti ostalih registrov pusti nespremenjene. Če funkcija deluje pravilno, bi moral spodnji program delovati kot tisti iz prejšnje naloge:

    main:
        ; pripravimo parametre funkcije
        MOV A, seznam
        MOV C, seznam
        ADD C, [dolzina]
        ; pokličemo funkcijo
        CALL poisci_minimum
        ; v mesto, na katerega kaže minimum, shranimo vrednost, na katero kaže B
        ; ker tega ne moremo narediti direktno, si pomagamo z registrom C
        PUSH C 
        MOV C, [B]
        MOV [minimum], C
        POP C
        HLT

    poisci_minimum:
	    PUSH A
	    MOV B, [A]
	    INC A
	    loop:
	    CMP B, [A]
	    JNC manj
	    back:
	    INC A
	    CMP A, C
	    JNC fin
	    JMP loop

	    manj:
	    MOV B, [A]
	    MOV D, A
	    JMP back
    
	    fin:
	    POP A
	    MOV B, D
	    RET

## Urejanje seznama

Zapišite funkcijo `uredi`, ki elemente v rezini [A:C] uredi od najmanjšega do največjega. Pri tem naj vrednosti vseh registrov pusti pri miru. Eden najenostavnejših algoritmov za urejanje je urejanje z izbiranjem. V njem se zapeljete čez seznam, poiščete indeks najmanjšega elementa, nato pa ta element zamenjate s tistim na prvem mestu. Postopek nadaljujete s preostankom seznama, dokler ne pridete do konca.

Delovanje lahko preverite s sledečim programom:

    main:
                            ; pripravimo argumente za funkcijo uredi
        MOV A, seznam       ; register A mora kazati na prvi element
        MOV C, seznam       ; register C mora kazati na zadnji element + 1
        ADD C, [dolzina]
        CALL uredi          ; pokličemo funkcijo za urejanje
        HLT                 ; prekinemo izvajanje

    uredi:
    	CALL poisci_minimum
    	PUSH [A]
    	MOV D, [B]
    	MOV [A], D
    	POP D
    	MOV [B], D
    	INC A
    	CMP A, C
    	JZ end
    	JMP uredi
    
    	end:
    	RET
	