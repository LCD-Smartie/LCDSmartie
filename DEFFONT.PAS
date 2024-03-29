unit DEFFONT;

{$MODE Delphi}

interface

const
  CharWidth = 5;
  CharHeight = 8;

const
  DefaultFont : array[32..255,0..CharHeight-1] of byte = (
    ($00,$00,$00,$00,$00,$00,$00,$00), // char 32
    ($04,$04,$04,$04,$00,$00,$04,$00), // char 33
    ($0A,$0A,$0A,$00,$00,$00,$00,$00), // char 34
    ($0A,$0A,$1F,$0A,$1F,$0A,$0A,$00), // char 35
    ($04,$0F,$14,$0E,$05,$1E,$04,$00), // char 36
    ($18,$19,$02,$04,$08,$13,$03,$00), // char 37
    ($0C,$12,$14,$08,$15,$12,$0D,$00), // char 38
    ($0C,$04,$08,$00,$00,$00,$00,$00), // char 39
    ($02,$04,$08,$08,$08,$04,$02,$00), // char 40
    ($08,$04,$02,$02,$02,$04,$08,$00), // char 41
    ($00,$04,$15,$0E,$15,$04,$00,$00), // char 42
    ($00,$04,$04,$1F,$04,$04,$00,$00), // char 43
    ($00,$00,$00,$00,$0C,$04,$08,$00), // char 44
    ($00,$00,$00,$1F,$00,$00,$00,$00), // char 45
    ($00,$00,$00,$00,$00,$0C,$0C,$00), // char 46
    ($00,$01,$02,$04,$08,$10,$00,$00), // char 47
    ($0E,$11,$13,$15,$19,$11,$0E,$00), // char 48
    ($04,$0C,$04,$04,$04,$04,$0E,$00), // char 49
    ($0E,$11,$01,$02,$04,$08,$1F,$00), // char 50
    ($1F,$02,$04,$02,$01,$11,$0E,$00), // char 51
    ($02,$06,$0A,$12,$1F,$02,$02,$00), // char 52
    ($1F,$10,$1E,$01,$01,$11,$0E,$00), // char 53
    ($06,$08,$10,$1E,$11,$11,$0E,$00), // char 54
    ($1F,$01,$02,$04,$08,$08,$08,$00), // char 55
    ($0E,$11,$11,$0E,$11,$11,$0E,$00), // char 56
    ($0E,$11,$11,$0F,$01,$02,$0C,$00), // char 57
    ($00,$0C,$0C,$00,$0C,$0C,$00,$00), // char 58
    ($00,$0C,$0C,$00,$0C,$04,$08,$00), // char 59
    ($02,$04,$08,$10,$08,$04,$02,$00), // char 60
    ($00,$00,$1F,$00,$1F,$00,$00,$00), // char 61
    ($08,$04,$02,$01,$02,$04,$08,$00), // char 62
    ($0E,$11,$01,$02,$04,$00,$04,$00), // char 63
    ($0E,$11,$01,$0D,$15,$15,$0E,$00), // char 64
    ($0E,$11,$11,$11,$1F,$11,$11,$00), // char 65
    ($1E,$11,$11,$1E,$11,$11,$1E,$00), // char 66
    ($0E,$11,$10,$10,$10,$11,$0E,$00), // char 67
    ($1C,$12,$11,$11,$11,$12,$1C,$00), // char 68
    ($1F,$10,$10,$1E,$10,$10,$1F,$00), // char 69
    ($1F,$10,$10,$1E,$10,$10,$10,$00), // char 70
    ($0E,$11,$10,$17,$11,$11,$0F,$00), // char 71
    ($11,$11,$11,$1F,$11,$11,$11,$00), // char 72
    ($0E,$04,$04,$04,$04,$04,$0E,$00), // char 73
    ($07,$02,$02,$02,$02,$12,$0C,$00), // char 74
    ($11,$12,$14,$18,$14,$12,$11,$00), // char 75
    ($10,$10,$10,$10,$10,$10,$1F,$00), // char 76
    ($11,$1B,$15,$15,$11,$11,$11,$00), // char 77
    ($11,$11,$19,$15,$13,$11,$11,$00), // char 78
    ($0E,$11,$11,$11,$11,$11,$0E,$00), // char 79
    ($1E,$11,$11,$1E,$10,$10,$10,$00), // char 80
    ($0E,$11,$11,$11,$15,$12,$0D,$00), // char 81
    ($1E,$11,$11,$1E,$14,$12,$11,$00), // char 82
    ($0F,$10,$10,$0E,$01,$01,$1E,$00), // char 83
    ($1F,$04,$04,$04,$04,$04,$04,$00), // char 84
    ($11,$11,$11,$11,$11,$11,$0E,$00), // char 85
    ($11,$11,$11,$11,$11,$0A,$04,$00), // char 86
    ($11,$11,$11,$11,$15,$15,$0A,$00), // char 87
    ($11,$11,$0A,$04,$0A,$11,$11,$00), // char 88
    ($11,$11,$11,$0A,$04,$04,$04,$00), // char 89
    ($1F,$01,$02,$04,$08,$10,$1F,$00), // char 90
    ($0E,$08,$08,$08,$08,$08,$0E,$00), // char 91
    ($00,$10,$08,$04,$02,$01,$00,$00), // char 92
    ($0E,$02,$02,$02,$02,$02,$0E,$00), // char 93
    ($04,$0A,$11,$00,$00,$00,$00,$00), // char 94
    ($00,$00,$00,$00,$00,$00,$1F,$00), // char 95
    ($08,$04,$02,$00,$00,$00,$00,$00), // char 96
    ($00,$00,$0E,$01,$0F,$11,$0F,$00), // char 97
    ($10,$10,$16,$19,$11,$11,$1E,$00), // char 98
    ($00,$00,$0E,$10,$10,$11,$0E,$00), // char 99
    ($01,$01,$0D,$13,$11,$11,$0F,$00), // char 100
    ($00,$00,$0E,$11,$1F,$10,$0E,$00), // char 101
    ($06,$09,$08,$1C,$08,$08,$08,$00), // char 102
    ($00,$0F,$11,$11,$0F,$01,$0E,$00), // char 103
    ($10,$10,$16,$19,$11,$11,$11,$00), // char 104
    ($04,$00,$0C,$04,$04,$04,$0E,$00), // char 105
    ($02,$00,$06,$02,$02,$12,$0C,$00), // char 106
    ($10,$10,$12,$14,$18,$14,$12,$00), // char 107
    ($0C,$04,$04,$04,$04,$04,$0E,$00), // char 108
    ($00,$00,$1A,$15,$15,$11,$11,$00), // char 109
    ($00,$00,$16,$19,$11,$11,$11,$00), // char 110
    ($00,$00,$0E,$11,$11,$11,$0E,$00), // char 111
    ($00,$00,$1E,$11,$1E,$10,$10,$00), // char 112
    ($00,$00,$0D,$13,$0F,$01,$01,$00), // char 113
    ($00,$00,$16,$19,$10,$10,$10,$00), // char 114
    ($00,$00,$0E,$10,$0E,$01,$1E,$00), // char 115
    ($08,$08,$1C,$08,$08,$09,$06,$00), // char 116
    ($00,$00,$11,$11,$11,$13,$0D,$00), // char 117
    ($00,$00,$11,$11,$11,$0A,$04,$00), // char 118
    ($00,$00,$11,$11,$15,$15,$0A,$00), // char 119
    ($00,$00,$11,$0A,$04,$0A,$11,$00), // char 120
    ($00,$00,$11,$11,$0F,$01,$0E,$00), // char 121
    ($00,$00,$1F,$02,$04,$08,$1F,$00), // char 122
    ($02,$04,$04,$08,$04,$04,$02,$00), // char 123
    ($04,$04,$04,$04,$04,$04,$04,$00), // char 124
    ($08,$04,$04,$02,$04,$04,$08,$00), // char 125
    ($00,$09,$16,$00,$00,$00,$00,$00), // char 126
    ($00,$04,$08,$1F,$08,$04,$00,$00), // char 127
    ($00,$00,$00,$00,$00,$00,$00,$00), // char 128 // extended stuff
    ($04,$04,$04,$04,$00,$00,$04,$00), // char 129
    ($0A,$0A,$0A,$00,$00,$00,$00,$00), // char 130
    ($0A,$0A,$1F,$0A,$1F,$0A,$0A,$00), // char 131 // custom
    ($04,$0F,$14,$0E,$05,$1E,$04,$00), // char 132 // custom
    ($18,$19,$02,$04,$08,$13,$03,$00), // char 133 // custom
    ($0C,$12,$14,$08,$15,$12,$0D,$00), // char 134 // custom
    ($0C,$04,$08,$00,$00,$00,$00,$00), // char 135 // custom
    ($02,$04,$08,$08,$08,$04,$02,$00), // char 136 // custom
    ($08,$04,$02,$02,$02,$04,$08,$00), // char 137
    ($00,$04,$15,$0E,$15,$04,$00,$00), // char 138
    ($00,$04,$04,$1F,$04,$04,$00,$00), // char 139
    ($00,$00,$00,$00,$0C,$04,$08,$00), // char 140
    ($00,$00,$00,$1F,$00,$00,$00,$00), // char 141
    ($00,$00,$00,$00,$00,$0C,$0C,$00), // char 142
    ($00,$01,$02,$04,$08,$10,$00,$00), // char 143
    ($0E,$11,$13,$15,$19,$11,$0E,$00), // char 144
    ($04,$0C,$04,$04,$04,$04,$0E,$00), // char 145
    ($0E,$11,$01,$02,$04,$08,$1F,$00), // char 146
    ($1F,$02,$04,$02,$01,$11,$0E,$00), // char 147
    ($02,$06,$0A,$12,$1F,$02,$02,$00), // char 148
    ($1F,$10,$1E,$01,$01,$11,$0E,$00), // char 149
    ($06,$08,$10,$1E,$11,$11,$0E,$00), // char 150
    ($1F,$01,$02,$04,$08,$08,$08,$00), // char 151
    ($0E,$11,$11,$0E,$11,$11,$0E,$00), // char 152
    ($0E,$11,$11,$0F,$01,$02,$0C,$00), // char 153
    ($00,$0C,$0C,$00,$0C,$0C,$00,$00), // char 154
    ($00,$0C,$0C,$00,$0C,$04,$08,$00), // char 155
    ($02,$04,$08,$10,$08,$04,$02,$00), // char 156
    ($00,$00,$1F,$00,$1F,$00,$00,$00), // char 157
    ($08,$04,$02,$01,$02,$04,$08,$00), // char 158 // custom
    ($0E,$11,$01,$02,$04,$00,$04,$00), // char 159
    ($0E,$11,$01,$0D,$15,$15,$0E,$00), // char 160
    ($0E,$11,$11,$11,$1F,$11,$11,$00), // char 161
    ($1E,$11,$11,$1E,$11,$11,$1E,$00), // char 162
    ($0E,$11,$10,$10,$10,$11,$0E,$00), // char 163
    ($1C,$12,$11,$11,$11,$12,$1C,$00), // char 164
    ($1F,$10,$10,$1E,$10,$10,$1F,$00), // char 165
    ($1F,$10,$10,$1E,$10,$10,$10,$00), // char 166
    ($0E,$11,$10,$17,$11,$11,$0F,$00), // char 167
    ($11,$11,$11,$1F,$11,$11,$11,$00), // char 168
    ($0E,$04,$04,$04,$04,$04,$0E,$00), // char 169
    ($07,$02,$02,$02,$02,$12,$0C,$00), // char 170
    ($11,$12,$14,$18,$14,$12,$11,$00), // char 171
    ($10,$10,$10,$10,$10,$10,$1F,$00), // char 172
    ($11,$1B,$15,$15,$11,$11,$11,$00), // char 173
    ($11,$11,$19,$15,$13,$11,$11,$00), // char 174
    ($0E,$11,$11,$11,$11,$11,$0E,$00), // char 175
    ($1E,$11,$11,$1E,$10,$10,$10,$00), // char 176 // custom
    ($0E,$11,$11,$11,$15,$12,$0D,$00), // char 177
    ($1E,$11,$11,$1E,$14,$12,$11,$00), // char 178
    ($0F,$10,$10,$0E,$01,$01,$1E,$00), // char 179
    ($1F,$04,$04,$04,$04,$04,$04,$00), // char 180
    ($11,$11,$11,$11,$11,$11,$0E,$00), // char 181
    ($11,$11,$11,$11,$11,$0A,$04,$00), // char 182
    ($11,$11,$11,$11,$15,$15,$0A,$00), // char 183
    ($11,$11,$0A,$04,$0A,$11,$11,$00), // char 184
    ($11,$11,$11,$0A,$04,$04,$04,$00), // char 185
    ($1F,$01,$02,$04,$08,$10,$1F,$00), // char 186
    ($0E,$08,$08,$08,$08,$08,$0E,$00), // char 187
    ($00,$10,$08,$04,$02,$01,$00,$00), // char 188
    ($0E,$02,$02,$02,$02,$02,$0E,$00), // char 189
    ($04,$0A,$11,$00,$00,$00,$00,$00), // char 190
    ($00,$00,$00,$00,$00,$00,$1F,$00), // char 191
    ($0E,$0A,$0E,$00,$00,$00,$00,$00), // char 192
    ($00,$00,$0E,$01,$0F,$11,$0F,$00), // char 193
    ($10,$10,$16,$19,$11,$11,$1E,$00), // char 194
    ($00,$00,$0E,$10,$10,$11,$0E,$00), // char 195
    ($01,$01,$0D,$13,$11,$11,$0F,$00), // char 196
    ($00,$00,$0E,$11,$1F,$10,$0E,$00), // char 197
    ($06,$09,$08,$1C,$08,$08,$08,$00), // char 198
    ($00,$0F,$11,$11,$0F,$01,$0E,$00), // char 199
    ($10,$10,$16,$19,$11,$11,$11,$00), // char 200
    ($04,$00,$0C,$04,$04,$04,$0E,$00), // char 201
    ($02,$00,$06,$02,$02,$12,$0C,$00), // char 202
    ($10,$10,$12,$14,$18,$14,$12,$00), // char 203
    ($0C,$04,$04,$04,$04,$04,$0E,$00), // char 204
    ($00,$00,$1A,$15,$15,$11,$11,$00), // char 205
    ($00,$00,$16,$19,$11,$11,$11,$00), // char 206
    ($00,$00,$0E,$11,$11,$11,$0E,$00), // char 207
    ($00,$00,$1E,$11,$1E,$10,$10,$00), // char 208
    ($00,$00,$0D,$13,$0F,$01,$01,$00), // char 209
    ($00,$00,$16,$19,$10,$10,$10,$00), // char 210
    ($00,$00,$0E,$10,$0E,$01,$1E,$00), // char 211
    ($08,$08,$1C,$08,$08,$09,$06,$00), // char 212
    ($00,$00,$11,$11,$11,$13,$0D,$00), // char 213
    ($00,$00,$11,$11,$11,$0A,$04,$00), // char 214
    ($00,$00,$11,$11,$15,$15,$0A,$00), // char 215
    ($00,$00,$11,$0A,$04,$0A,$11,$00), // char 216
    ($00,$00,$11,$11,$0F,$01,$0E,$00), // char 217
    ($00,$00,$1F,$02,$04,$08,$1F,$00), // char 218
    ($02,$04,$04,$08,$04,$04,$02,$00), // char 219
    ($04,$04,$04,$04,$04,$04,$04,$00), // char 220
    ($08,$04,$04,$02,$04,$04,$08,$00), // char 221
    ($00,$09,$16,$00,$00,$00,$00,$00), // char 222
    ($1C,$14,$1C,$00,$00,$00,$00,$00), // char 223 // square degree
    ($00,$00,$00,$00,$00,$00,$00,$00), // char 224
    ($04,$04,$04,$04,$00,$00,$04,$00), // char 225
    ($0A,$0A,$0A,$00,$00,$00,$00,$00), // char 226
    ($0A,$0A,$1F,$0A,$1F,$0A,$0A,$00), // char 227
    ($04,$0F,$14,$0E,$05,$1E,$04,$00), // char 228
    ($18,$19,$02,$04,$08,$13,$03,$00), // char 229
    ($0C,$12,$14,$08,$15,$12,$0D,$00), // char 230
    ($0C,$04,$08,$00,$00,$00,$00,$00), // char 231
    ($02,$04,$08,$08,$08,$04,$02,$00), // char 232
    ($08,$04,$02,$02,$02,$04,$08,$00), // char 233
    ($00,$04,$15,$0E,$15,$04,$00,$00), // char 234
    ($00,$04,$04,$1F,$04,$04,$00,$00), // char 235
    ($00,$00,$00,$00,$0C,$04,$08,$00), // char 236
    ($00,$00,$00,$1F,$00,$00,$00,$00), // char 237
    ($00,$00,$00,$00,$00,$0C,$0C,$00), // char 238
    ($00,$01,$02,$04,$08,$10,$00,$00), // char 239
    ($0E,$11,$13,$15,$19,$11,$0E,$00), // char 240
    ($04,$0C,$04,$04,$04,$04,$0E,$00), // char 241
    ($0E,$11,$01,$02,$04,$08,$1F,$00), // char 242
    ($1F,$02,$04,$02,$01,$11,$0E,$00), // char 243
    ($02,$06,$0A,$12,$1F,$02,$02,$00), // char 244
    ($1F,$10,$1E,$01,$01,$11,$0E,$00), // char 245
    ($06,$08,$10,$1E,$11,$11,$0E,$00), // char 246
    ($1F,$01,$02,$04,$08,$08,$08,$00), // char 247
    ($0E,$11,$11,$0E,$11,$11,$0E,$00), // char 248
    ($0E,$11,$11,$0F,$01,$02,$0C,$00), // char 249
    ($00,$0C,$0C,$00,$0C,$0C,$00,$00), // char 250
    ($00,$0C,$0C,$00,$0C,$04,$08,$00), // char 251
    ($02,$04,$08,$10,$08,$04,$02,$00), // char 252
    ($00,$00,$1F,$00,$1F,$00,$00,$00), // char 253
    ($08,$04,$02,$01,$02,$04,$08,$00), // char 254
    ($1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F)); // char 255 // full block

implementation

end.
