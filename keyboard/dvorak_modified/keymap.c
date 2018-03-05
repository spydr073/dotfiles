
#include "planck.h"
//#include "process_unicode_common.h"
//#include "action_layer.h"

//extern keymap_config_t keymap_config;

/*  --- Layer Modifiers ---
 *  M(n)   Perform Macro n
 *  F(n)   Perform Function n
 *
 *  TO(x)  GOTO layer x
 *  MO(x)  Momentary switch to x
 *  DF(x)  Set defalut layer to x
 *  TG(x)  Toggle to layer x
 *  OSL(x) One-shot layer x
 *  TT(x)  Tap-toggle layer x
 */


//-- Initialize
void matrix_init_user(void) {
    set_unicode_input_mode(UC_LNX); //-- options are LNX, OSX, or WINC
};



const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    /* Modified Dvorak (0)
     * ,-----------------------------------------x-----------------------------------------.
     * | Esc  |   "  |   ,  |   .  |   p  |   y  |   f  |   g  |   c  |   r  |   l  | BSPC |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * | Tab  |   a  |   o  |   e  |   u  |   i  |   d  |   h  |   t  |   n  |   s  |  /   |
     * |------+------+------+------+------+------X------+------+------+------+------+------|
     * |  ~   |   ;  |   q  |   j  |   k  |   x  |   b  |   m  |   w  |   v  |   z  |  \   |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * | VolDn| MO(3)| ALT  | GUI  | MO(2)|Space | Enter| Shift| MO(5)| CTL  | MO(6)| VolUp|
     * `-----------------------------------------x-----------------------------------------'
     */
    [0] = { //-- Default Layout
      {KC_ESC,  KC_DQUO, KC_COMM, KC_DOT,  KC_P,   KC_Y,   KC_F,   KC_G,  KC_C,   KC_R,    KC_L,  KC_BSPC},
      {KC_TAB,  KC_A,    KC_O,    KC_E,    KC_U,   KC_I,   KC_D,   KC_H,  KC_T,   KC_N,    KC_S,  KC_BSLS},
      {KC_TILD, KC_SCLN, KC_Q,    KC_J,    KC_K,   KC_X,   KC_B,   KC_M,  KC_W,   KC_V,    KC_Z,  KC_SLSH},
      {KC_VOLD, MO(3),   KC_LALT, KC_LGUI, MO(2),  KC_SPC, KC_ENT, M(0),  MO(5),  KC_LCTL, MO(6), KC_VOLU}
    },

    /* Shifted Layout (1)
     * ,-----------------------------------------x-----------------------------------------.
     * |SIGINT|   '  |   _  |   -  |   P  |   Y  |   F  |   G  |   C  |   R  |   L  | Del  |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * | EOF  |   A  |   O  |   E  |   U  |   I  |   D  |   H  |   T  |   N  |   S  |  ?   |
     * |------+------+------+------+------+------X------+------+------+------+------+------|
     * |  `   |   :  |   Q  |   J  |   K  |   X  |   B  |   M  |   W  |   V  |   Z  |  |   |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * | HOME |      | ALT  |      |      |Space | Enter|      |      | CTL  |      | END  |
     * `-----------------------------------------x-----------------------------------------'
     */
    [1] = { //-- Shifted Chars
      {M(1), M(5),    M(6),    M(7),    KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, M(8)},
      {M(2), KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS},
      {M(3), KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS},
      {M(4), KC_NO,   KC_TRNS, KC_TRNS, KC_NO,   KC_TRNS, KC_TRNS, KC_TRNS, KC_NO,   KC_TRNS, KC_NO,   M(9)}
    },

    /* Numeric Layout (2)
     * ,-----------------------------------------x-----------------------------------------.
     * |      |   ≤  |   ≥  |   [  |  ]   |  =   |   ⨯  |   *  |   7  |   8  |  9   | BSPC |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |   <  |   >  |   (  |  )   |  ±   |   -  |   +  |   4  |   5  |  6   |  .   |
     * |------+------+------+------+------+------X------+------+------+------+------+------|
     * |      |   ≈  |   ≡  |   {  |  }   |  ≠   |   ÷  |   /  |   1  |   2  |  3   |  0   |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * | PGUP |      | ALT  |      |      |Space | Enter|      |      | CTL  |      | PGDN |
     * `-----------------------------------------x-----------------------------------------'
     */
    [2] = { //-- Numeric Chars
      {KC_NO,   UC(0x2264), UC(0x2265), KC_LBRC, KC_RBRC, KC_EQL,     UC(0x00D7), KC_ASTR, KC_7,  KC_8,    KC_9,  KC_BSPC},
      {KC_NO,   KC_LABK,    KC_RABK,    KC_LPRN, KC_RPRN, UC(0x00B1), KC_MINS,    KC_PLUS, KC_4,  KC_5,    KC_6,  KC_DOT},
      {KC_NO,   UC(0x2248), UC(0x2261), KC_LCBR, KC_RCBR, UC(0x2260), UC(0x00F7), KC_SLSH, KC_1,  KC_2,    KC_3,  KC_0},
      {KC_PGUP, KC_NO,      KC_TRNS,    KC_TRNS, KC_TRNS, KC_TRNS,    KC_TRNS,    KC_NO,   KC_NO, KC_LCTL, KC_NO, KC_PGDN}
    },

    /* Greek Lower Layout (3)
     * ,-----------------------------------------x-----------------------------------------.
     * |      |      |      |   ∂  |   π  |   ψ  |   φ  |   γ  |   χ  |   ρ  |   λ  |      |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |   α  |   ο  |   ε  |   u  |   ι  |   δ  |   η  |   τ  |   ν  |   σ  |      |
     * |------+------+------+------+------+------X------+------+------+------+------+------|
     * |      |      |      |   θ  |   κ  |   ξ  |   β  |   μ  |   ω  |   ς  |   ζ  |      |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |      | Alt  |      |      |Space | Enter| F(4) |      | CTL  |      |      |
     * `-----------------------------------------x-----------------------------------------'
     */
    [3] = { //-- Greek Chars
      {KC_NO, KC_NO,      KC_NO,      UC(0x2202), UC(0x03C0), UC(0x03C8), UC(0x03C6), UC(0x03B3), UC(0x03C7), UC(0x03C1), UC(0x03Bb), KC_NO},
      {KC_NO, UC(0x03B1), UC(0x03BF), UC(0x03B5), UC(0x03B5), UC(0x03B9), UC(0x03B4), UC(0x03B7), UC(0x03C4), UC(0x03BD), UC(0x03C3), KC_NO},
      {KC_NO, KC_NO,      KC_NO,      UC(0x03B8), UC(0x03BA), UC(0x03BE), UC(0x03B2), UC(0x03BC), UC(0x03C9), UC(0x03C2), UC(0x03B6), KC_NO},
      {KC_NO, KC_TRNS,    KC_TRNS,    KC_TRNS,    KC_NO,      KC_TRNS,    KC_TRNS,    F(4),       KC_NO,      KC_TRNS,    KC_NO,      KC_NO}
    },

    /* Greek Upper Layout (4)
     * ,-----------------------------------------x-----------------------------------------.
     * |      |      |      |   ∇  |   Π  |   Ψ  |   Φ  |   Γ  |   Χ  |   Ρ  |   Λ  |      |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |   Α  |   Ο  |   Ε  |   Υ  |   Ι  |   Δ  |   Η  |   Τ  |   Ν  |   Σ  |      |
     * |------+------+------+------+------+------X------+------+------+------+------+------|
     * |      |      |      |   Θ  |   Κ  |   Ξ  |   Β  |   Μ  |   Ω  |   Σ  |   Ζ  |      |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |      | Alt  |      |      |Space | Enter|      |      | CTL  |      |      |
     * `-----------------------------------------x-----------------------------------------'
     */
    [4] = { //-- Shifted Greek Chars
      {KC_NO, KC_NO,      KC_NO,      UC(0x2207), UC(0x03A0), UC(0x03A8), UC(0x03A6), UC(0x0393), UC(0x03A7), UC(0x03A1), UC(0x039b), KC_NO},
      {KC_NO, UC(0x0391), UC(0x039F), UC(0x0395), UC(0x03A5), UC(0x0399), UC(0x0394), UC(0x0397), UC(0x03A4), UC(0x039D), UC(0x03A3), KC_NO},
      {KC_NO, KC_NO,      KC_NO,      UC(0x0398), UC(0x039A), UC(0x033E), UC(0x0392), UC(0x039C), UC(0x03A9), UC(0x03A3), UC(0x0336), KC_NO},
      {KC_NO, KC_NO,      KC_TRNS,    KC_TRNS,    KC_NO,      KC_TRNS,    KC_TRNS,    KC_TRNS,    KC_NO,      KC_TRNS,    KC_NO,      KC_NO}
    },

    /* Symbolic Layout (5)
     * ,-----------------------------------------x-----------------------------------------.
     * |      |   ∀  |  ∃   |   ¬  |  ∧   |  ∨   |  ⊥   |   ⊢  |   ⇓  |  ∴   |   ∎  |      |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |   ⇔  |  ⇒   |   ↦  |  →   |  ←   |  ∘   |   ∙  |      |      |      |      |
     * |------+------+------+------+------+------X------+------+------+------+------+------|
     * |      |   ⊆  |  ∈   |   ∉  |  ∅   |  ∩   |  ∪   |   ∝  |   ∫  |  ∵   |      |      |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |      | Alt  |      |      |Space | Enter|      |      | CTL  |      |      |
     * `-----------------------------------------x-----------------------------------------'
    */
    [5] = { //-- Symbolic Chars
      {KC_NO, UC(0x2200), UC(0x2203), UC(0x00AC), UC(0x2227), UC(0x2228), UC(0x22A5), UC(0x22A2), UC(0x21D3), UC(0x2234), UC(0x220E), KC_NO},
      {KC_NO, UC(0x21D4), UC(0x21D2), UC(0x21A6), UC(0x2192), UC(0x2190), UC(0x2218), UC(0x2219), KC_NO,      KC_NO,      KC_NO,      KC_NO},
      {KC_NO, UC(0x2286), UC(0x2208), UC(0x2209), UC(0x2205), UC(0x22C2), UC(0x22C3), UC(0x221D), UC(0x222B), UC(0x2235), KC_NO,      KC_NO},
      {KC_NO, KC_NO,      KC_TRNS,    KC_TRNS,    KC_NO,      KC_TRNS,    KC_TRNS,    KC_NO,      KC_TRNS,    KC_TRNS,    KC_NO,      KC_NO}
    },

    /* Misc Layout (6)
     * ,-----------------------------------------x-----------------------------------------.
     * |      |      |      |  Up  |      |   #  |   !  |   $  |  f9  |  f10 |  f11 | f12  |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |      | Left | Down | Right|   &  |   %  |   €  |  f5  |  f6  |  f7  | f8   |
     * |------+------+------+------+------+------X------+------+------+------+------+------|
     * |      |      |      |      |      |   @  |   ^  |   °  |  f1  |  f2  |  f3  | f4   |
     * |------+------+------+------+------+------+------+------+------+------+------+------|
     * |      |      | Alt  |      |      |Space | Enter|      |      | CTL  |      |      |
     * `-----------------------------------------x-----------------------------------------'
     */
    [6] = { //-- FN-Misc Chars
      {KC_NO, KC_NO, KC_NO,   KC_UP,   KC_NO,    KC_HASH, KC_EXLM, KC_DLR,     KC_F9, KC_F10,  KC_F11,  KC_F12},
      {KC_NO, KC_NO, KC_LEFT, KC_DOWN, KC_RIGHT, KC_AMPR, KC_PERC, UC(0x20ac), KC_F5, KC_F6,   KC_F7,   KC_F8},
      {KC_NO, KC_NO, KC_NO,   KC_NO,   KC_NO,    KC_AT,   KC_CIRC, UC(0x00b0), KC_F1, KC_F2,   KC_F3,   KC_F4},
      {KC_NO, KC_NO, KC_TRNS, KC_TRNS, KC_NO,    KC_NO,   KC_TRNS, KC_TRNS,    KC_NO, KC_TRNS, KC_TRNS, KC_NO}
    }
};



//-- Functions
const uint16_t PROGMEM fn_actions[] = {
};


//-- Macros
const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt) {
    switch(id) {
        case 0: //-- Shift
            if (record->event.pressed) {
                register_code(KC_LSFT);
                layer_on(1);
            } else {
                unregister_code(KC_LSFT);
                layer_off(1);
            }
            break;
        case 1: //-- SigInt
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_LCTL);
                register_code(KC_C);
            } else {
                unregister_code(KC_LCTL);
                unregister_code(KC_C);
            }
            break;
        case 2: //-- EOF
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_LCTL);
                register_code(KC_D);
            } else {
                register_code(KC_LCTL);
                unregister_code(KC_D);
            }
            break;
        case 3: //-- Grave
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_GRV);
            } else {
                unregister_code(KC_GRV);
            }
            break;
        case 4: //-- Home
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_HOME);
            } else {
                unregister_code(KC_HOME);
            }
            break;
        case 5: //--  Single Quote
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_QUOT);
            } else {
                unregister_code(KC_QUOT);
            }
            break;
        case 6: //-- Underscore
            if (record->event.pressed) {
                //unregister_code(KC_LSFT);
                register_code(KC_MINS);
            } else {
                unregister_code(KC_MINS);
            }
            break;
        case 7: //-- Hyphen
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_MINS);
            } else {
                unregister_code(KC_MINS);
            }
            break;
        case 8: //-- Delete
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_DEL);
            } else {
                unregister_code(KC_DEL);
            }
            break;
        case 9: //-- End
            if (record->event.pressed) {
                unregister_code(KC_LSFT);
                register_code(KC_END);
            } else {
                unregister_code(KC_END);
            }
            break;
    }
    return MACRO_NONE;
};



