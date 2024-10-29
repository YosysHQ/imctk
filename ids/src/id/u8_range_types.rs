use crate::NoUninit;

#[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
#[derive(Clone, Copy, NoUninit)]
#[repr(u8)]
pub enum NonMaxU8 {
    Val00 = 0x00,
    Val01 = 0x01,
    Val02 = 0x02,
    Val03 = 0x03,
    Val04 = 0x04,
    Val05 = 0x05,
    Val06 = 0x06,
    Val07 = 0x07,
    Val08 = 0x08,
    Val09 = 0x09,
    Val0A = 0x0a,
    Val0B = 0x0b,
    Val0C = 0x0c,
    Val0D = 0x0d,
    Val0E = 0x0e,
    Val0F = 0x0f,
    Val10 = 0x10,
    Val11 = 0x11,
    Val12 = 0x12,
    Val13 = 0x13,
    Val14 = 0x14,
    Val15 = 0x15,
    Val16 = 0x16,
    Val17 = 0x17,
    Val18 = 0x18,
    Val19 = 0x19,
    Val1A = 0x1a,
    Val1B = 0x1b,
    Val1C = 0x1c,
    Val1D = 0x1d,
    Val1E = 0x1e,
    Val1F = 0x1f,
    Val20 = 0x20,
    Val21 = 0x21,
    Val22 = 0x22,
    Val23 = 0x23,
    Val24 = 0x24,
    Val25 = 0x25,
    Val26 = 0x26,
    Val27 = 0x27,
    Val28 = 0x28,
    Val29 = 0x29,
    Val2A = 0x2a,
    Val2B = 0x2b,
    Val2C = 0x2c,
    Val2D = 0x2d,
    Val2E = 0x2e,
    Val2F = 0x2f,
    Val30 = 0x30,
    Val31 = 0x31,
    Val32 = 0x32,
    Val33 = 0x33,
    Val34 = 0x34,
    Val35 = 0x35,
    Val36 = 0x36,
    Val37 = 0x37,
    Val38 = 0x38,
    Val39 = 0x39,
    Val3A = 0x3a,
    Val3B = 0x3b,
    Val3C = 0x3c,
    Val3D = 0x3d,
    Val3E = 0x3e,
    Val3F = 0x3f,
    Val40 = 0x40,
    Val41 = 0x41,
    Val42 = 0x42,
    Val43 = 0x43,
    Val44 = 0x44,
    Val45 = 0x45,
    Val46 = 0x46,
    Val47 = 0x47,
    Val48 = 0x48,
    Val49 = 0x49,
    Val4A = 0x4a,
    Val4B = 0x4b,
    Val4C = 0x4c,
    Val4D = 0x4d,
    Val4E = 0x4e,
    Val4F = 0x4f,
    Val50 = 0x50,
    Val51 = 0x51,
    Val52 = 0x52,
    Val53 = 0x53,
    Val54 = 0x54,
    Val55 = 0x55,
    Val56 = 0x56,
    Val57 = 0x57,
    Val58 = 0x58,
    Val59 = 0x59,
    Val5A = 0x5a,
    Val5B = 0x5b,
    Val5C = 0x5c,
    Val5D = 0x5d,
    Val5E = 0x5e,
    Val5F = 0x5f,
    Val60 = 0x60,
    Val61 = 0x61,
    Val62 = 0x62,
    Val63 = 0x63,
    Val64 = 0x64,
    Val65 = 0x65,
    Val66 = 0x66,
    Val67 = 0x67,
    Val68 = 0x68,
    Val69 = 0x69,
    Val6A = 0x6a,
    Val6B = 0x6b,
    Val6C = 0x6c,
    Val6D = 0x6d,
    Val6E = 0x6e,
    Val6F = 0x6f,
    Val70 = 0x70,
    Val71 = 0x71,
    Val72 = 0x72,
    Val73 = 0x73,
    Val74 = 0x74,
    Val75 = 0x75,
    Val76 = 0x76,
    Val77 = 0x77,
    Val78 = 0x78,
    Val79 = 0x79,
    Val7A = 0x7a,
    Val7B = 0x7b,
    Val7C = 0x7c,
    Val7D = 0x7d,
    Val7E = 0x7e,
    Val7F = 0x7f,
    Val80 = 0x80,
    Val81 = 0x81,
    Val82 = 0x82,
    Val83 = 0x83,
    Val84 = 0x84,
    Val85 = 0x85,
    Val86 = 0x86,
    Val87 = 0x87,
    Val88 = 0x88,
    Val89 = 0x89,
    Val8A = 0x8a,
    Val8B = 0x8b,
    Val8C = 0x8c,
    Val8D = 0x8d,
    Val8E = 0x8e,
    Val8F = 0x8f,
    Val90 = 0x90,
    Val91 = 0x91,
    Val92 = 0x92,
    Val93 = 0x93,
    Val94 = 0x94,
    Val95 = 0x95,
    Val96 = 0x96,
    Val97 = 0x97,
    Val98 = 0x98,
    Val99 = 0x99,
    Val9A = 0x9a,
    Val9B = 0x9b,
    Val9C = 0x9c,
    Val9D = 0x9d,
    Val9E = 0x9e,
    Val9F = 0x9f,
    ValA0 = 0xa0,
    ValA1 = 0xa1,
    ValA2 = 0xa2,
    ValA3 = 0xa3,
    ValA4 = 0xa4,
    ValA5 = 0xa5,
    ValA6 = 0xa6,
    ValA7 = 0xa7,
    ValA8 = 0xa8,
    ValA9 = 0xa9,
    ValAA = 0xaa,
    ValAB = 0xab,
    ValAC = 0xac,
    ValAD = 0xad,
    ValAE = 0xae,
    ValAF = 0xaf,
    ValB0 = 0xb0,
    ValB1 = 0xb1,
    ValB2 = 0xb2,
    ValB3 = 0xb3,
    ValB4 = 0xb4,
    ValB5 = 0xb5,
    ValB6 = 0xb6,
    ValB7 = 0xb7,
    ValB8 = 0xb8,
    ValB9 = 0xb9,
    ValBA = 0xba,
    ValBB = 0xbb,
    ValBC = 0xbc,
    ValBD = 0xbd,
    ValBE = 0xbe,
    ValBF = 0xbf,
    ValC0 = 0xc0,
    ValC1 = 0xc1,
    ValC2 = 0xc2,
    ValC3 = 0xc3,
    ValC4 = 0xc4,
    ValC5 = 0xc5,
    ValC6 = 0xc6,
    ValC7 = 0xc7,
    ValC8 = 0xc8,
    ValC9 = 0xc9,
    ValCA = 0xca,
    ValCB = 0xcb,
    ValCC = 0xcc,
    ValCD = 0xcd,
    ValCE = 0xce,
    ValCF = 0xcf,
    ValD0 = 0xd0,
    ValD1 = 0xd1,
    ValD2 = 0xd2,
    ValD3 = 0xd3,
    ValD4 = 0xd4,
    ValD5 = 0xd5,
    ValD6 = 0xd6,
    ValD7 = 0xd7,
    ValD8 = 0xd8,
    ValD9 = 0xd9,
    ValDA = 0xda,
    ValDB = 0xdb,
    ValDC = 0xdc,
    ValDD = 0xdd,
    ValDE = 0xde,
    ValDF = 0xdf,
    ValE0 = 0xe0,
    ValE1 = 0xe1,
    ValE2 = 0xe2,
    ValE3 = 0xe3,
    ValE4 = 0xe4,
    ValE5 = 0xe5,
    ValE6 = 0xe6,
    ValE7 = 0xe7,
    ValE8 = 0xe8,
    ValE9 = 0xe9,
    ValEA = 0xea,
    ValEB = 0xeb,
    ValEC = 0xec,
    ValED = 0xed,
    ValEE = 0xee,
    ValEF = 0xef,
    ValF0 = 0xf0,
    ValF1 = 0xf1,
    ValF2 = 0xf2,
    ValF3 = 0xf3,
    ValF4 = 0xf4,
    ValF5 = 0xf5,
    ValF6 = 0xf6,
    ValF7 = 0xf7,
    ValF8 = 0xf8,
    ValF9 = 0xf9,
    ValFA = 0xfa,
    ValFB = 0xfb,
    ValFC = 0xfc,
    ValFD = 0xfd,
    ValFE = 0xfe,
}

#[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
#[derive(Clone, Copy, NoUninit)]
#[repr(u8)]
pub enum NonMaxHighNibbleU8 {
    Val00 = 0x00,
    Val01 = 0x01,
    Val02 = 0x02,
    Val03 = 0x03,
    Val04 = 0x04,
    Val05 = 0x05,
    Val06 = 0x06,
    Val07 = 0x07,
    Val08 = 0x08,
    Val09 = 0x09,
    Val0A = 0x0a,
    Val0B = 0x0b,
    Val0C = 0x0c,
    Val0D = 0x0d,
    Val0E = 0x0e,
    Val0F = 0x0f,
    Val10 = 0x10,
    Val11 = 0x11,
    Val12 = 0x12,
    Val13 = 0x13,
    Val14 = 0x14,
    Val15 = 0x15,
    Val16 = 0x16,
    Val17 = 0x17,
    Val18 = 0x18,
    Val19 = 0x19,
    Val1A = 0x1a,
    Val1B = 0x1b,
    Val1C = 0x1c,
    Val1D = 0x1d,
    Val1E = 0x1e,
    Val1F = 0x1f,
    Val20 = 0x20,
    Val21 = 0x21,
    Val22 = 0x22,
    Val23 = 0x23,
    Val24 = 0x24,
    Val25 = 0x25,
    Val26 = 0x26,
    Val27 = 0x27,
    Val28 = 0x28,
    Val29 = 0x29,
    Val2A = 0x2a,
    Val2B = 0x2b,
    Val2C = 0x2c,
    Val2D = 0x2d,
    Val2E = 0x2e,
    Val2F = 0x2f,
    Val30 = 0x30,
    Val31 = 0x31,
    Val32 = 0x32,
    Val33 = 0x33,
    Val34 = 0x34,
    Val35 = 0x35,
    Val36 = 0x36,
    Val37 = 0x37,
    Val38 = 0x38,
    Val39 = 0x39,
    Val3A = 0x3a,
    Val3B = 0x3b,
    Val3C = 0x3c,
    Val3D = 0x3d,
    Val3E = 0x3e,
    Val3F = 0x3f,
    Val40 = 0x40,
    Val41 = 0x41,
    Val42 = 0x42,
    Val43 = 0x43,
    Val44 = 0x44,
    Val45 = 0x45,
    Val46 = 0x46,
    Val47 = 0x47,
    Val48 = 0x48,
    Val49 = 0x49,
    Val4A = 0x4a,
    Val4B = 0x4b,
    Val4C = 0x4c,
    Val4D = 0x4d,
    Val4E = 0x4e,
    Val4F = 0x4f,
    Val50 = 0x50,
    Val51 = 0x51,
    Val52 = 0x52,
    Val53 = 0x53,
    Val54 = 0x54,
    Val55 = 0x55,
    Val56 = 0x56,
    Val57 = 0x57,
    Val58 = 0x58,
    Val59 = 0x59,
    Val5A = 0x5a,
    Val5B = 0x5b,
    Val5C = 0x5c,
    Val5D = 0x5d,
    Val5E = 0x5e,
    Val5F = 0x5f,
    Val60 = 0x60,
    Val61 = 0x61,
    Val62 = 0x62,
    Val63 = 0x63,
    Val64 = 0x64,
    Val65 = 0x65,
    Val66 = 0x66,
    Val67 = 0x67,
    Val68 = 0x68,
    Val69 = 0x69,
    Val6A = 0x6a,
    Val6B = 0x6b,
    Val6C = 0x6c,
    Val6D = 0x6d,
    Val6E = 0x6e,
    Val6F = 0x6f,
    Val70 = 0x70,
    Val71 = 0x71,
    Val72 = 0x72,
    Val73 = 0x73,
    Val74 = 0x74,
    Val75 = 0x75,
    Val76 = 0x76,
    Val77 = 0x77,
    Val78 = 0x78,
    Val79 = 0x79,
    Val7A = 0x7a,
    Val7B = 0x7b,
    Val7C = 0x7c,
    Val7D = 0x7d,
    Val7E = 0x7e,
    Val7F = 0x7f,
    Val80 = 0x80,
    Val81 = 0x81,
    Val82 = 0x82,
    Val83 = 0x83,
    Val84 = 0x84,
    Val85 = 0x85,
    Val86 = 0x86,
    Val87 = 0x87,
    Val88 = 0x88,
    Val89 = 0x89,
    Val8A = 0x8a,
    Val8B = 0x8b,
    Val8C = 0x8c,
    Val8D = 0x8d,
    Val8E = 0x8e,
    Val8F = 0x8f,
    Val90 = 0x90,
    Val91 = 0x91,
    Val92 = 0x92,
    Val93 = 0x93,
    Val94 = 0x94,
    Val95 = 0x95,
    Val96 = 0x96,
    Val97 = 0x97,
    Val98 = 0x98,
    Val99 = 0x99,
    Val9A = 0x9a,
    Val9B = 0x9b,
    Val9C = 0x9c,
    Val9D = 0x9d,
    Val9E = 0x9e,
    Val9F = 0x9f,
    ValA0 = 0xa0,
    ValA1 = 0xa1,
    ValA2 = 0xa2,
    ValA3 = 0xa3,
    ValA4 = 0xa4,
    ValA5 = 0xa5,
    ValA6 = 0xa6,
    ValA7 = 0xa7,
    ValA8 = 0xa8,
    ValA9 = 0xa9,
    ValAA = 0xaa,
    ValAB = 0xab,
    ValAC = 0xac,
    ValAD = 0xad,
    ValAE = 0xae,
    ValAF = 0xaf,
    ValB0 = 0xb0,
    ValB1 = 0xb1,
    ValB2 = 0xb2,
    ValB3 = 0xb3,
    ValB4 = 0xb4,
    ValB5 = 0xb5,
    ValB6 = 0xb6,
    ValB7 = 0xb7,
    ValB8 = 0xb8,
    ValB9 = 0xb9,
    ValBA = 0xba,
    ValBB = 0xbb,
    ValBC = 0xbc,
    ValBD = 0xbd,
    ValBE = 0xbe,
    ValBF = 0xbf,
    ValC0 = 0xc0,
    ValC1 = 0xc1,
    ValC2 = 0xc2,
    ValC3 = 0xc3,
    ValC4 = 0xc4,
    ValC5 = 0xc5,
    ValC6 = 0xc6,
    ValC7 = 0xc7,
    ValC8 = 0xc8,
    ValC9 = 0xc9,
    ValCA = 0xca,
    ValCB = 0xcb,
    ValCC = 0xcc,
    ValCD = 0xcd,
    ValCE = 0xce,
    ValCF = 0xcf,
    ValD0 = 0xd0,
    ValD1 = 0xd1,
    ValD2 = 0xd2,
    ValD3 = 0xd3,
    ValD4 = 0xd4,
    ValD5 = 0xd5,
    ValD6 = 0xd6,
    ValD7 = 0xd7,
    ValD8 = 0xd8,
    ValD9 = 0xd9,
    ValDA = 0xda,
    ValDB = 0xdb,
    ValDC = 0xdc,
    ValDD = 0xdd,
    ValDE = 0xde,
    ValDF = 0xdf,
    ValE0 = 0xe0,
    ValE1 = 0xe1,
    ValE2 = 0xe2,
    ValE3 = 0xe3,
    ValE4 = 0xe4,
    ValE5 = 0xe5,
    ValE6 = 0xe6,
    ValE7 = 0xe7,
    ValE8 = 0xe8,
    ValE9 = 0xe9,
    ValEA = 0xea,
    ValEB = 0xeb,
    ValEC = 0xec,
    ValED = 0xed,
    ValEE = 0xee,
    ValEF = 0xef,
}

#[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
#[derive(Clone, Copy, NoUninit)]
#[repr(u8)]
pub enum NonMaxMsbU8 {
    Val00 = 0x00,
    Val01 = 0x01,
    Val02 = 0x02,
    Val03 = 0x03,
    Val04 = 0x04,
    Val05 = 0x05,
    Val06 = 0x06,
    Val07 = 0x07,
    Val08 = 0x08,
    Val09 = 0x09,
    Val0A = 0x0a,
    Val0B = 0x0b,
    Val0C = 0x0c,
    Val0D = 0x0d,
    Val0E = 0x0e,
    Val0F = 0x0f,
    Val10 = 0x10,
    Val11 = 0x11,
    Val12 = 0x12,
    Val13 = 0x13,
    Val14 = 0x14,
    Val15 = 0x15,
    Val16 = 0x16,
    Val17 = 0x17,
    Val18 = 0x18,
    Val19 = 0x19,
    Val1A = 0x1a,
    Val1B = 0x1b,
    Val1C = 0x1c,
    Val1D = 0x1d,
    Val1E = 0x1e,
    Val1F = 0x1f,
    Val20 = 0x20,
    Val21 = 0x21,
    Val22 = 0x22,
    Val23 = 0x23,
    Val24 = 0x24,
    Val25 = 0x25,
    Val26 = 0x26,
    Val27 = 0x27,
    Val28 = 0x28,
    Val29 = 0x29,
    Val2A = 0x2a,
    Val2B = 0x2b,
    Val2C = 0x2c,
    Val2D = 0x2d,
    Val2E = 0x2e,
    Val2F = 0x2f,
    Val30 = 0x30,
    Val31 = 0x31,
    Val32 = 0x32,
    Val33 = 0x33,
    Val34 = 0x34,
    Val35 = 0x35,
    Val36 = 0x36,
    Val37 = 0x37,
    Val38 = 0x38,
    Val39 = 0x39,
    Val3A = 0x3a,
    Val3B = 0x3b,
    Val3C = 0x3c,
    Val3D = 0x3d,
    Val3E = 0x3e,
    Val3F = 0x3f,
    Val40 = 0x40,
    Val41 = 0x41,
    Val42 = 0x42,
    Val43 = 0x43,
    Val44 = 0x44,
    Val45 = 0x45,
    Val46 = 0x46,
    Val47 = 0x47,
    Val48 = 0x48,
    Val49 = 0x49,
    Val4A = 0x4a,
    Val4B = 0x4b,
    Val4C = 0x4c,
    Val4D = 0x4d,
    Val4E = 0x4e,
    Val4F = 0x4f,
    Val50 = 0x50,
    Val51 = 0x51,
    Val52 = 0x52,
    Val53 = 0x53,
    Val54 = 0x54,
    Val55 = 0x55,
    Val56 = 0x56,
    Val57 = 0x57,
    Val58 = 0x58,
    Val59 = 0x59,
    Val5A = 0x5a,
    Val5B = 0x5b,
    Val5C = 0x5c,
    Val5D = 0x5d,
    Val5E = 0x5e,
    Val5F = 0x5f,
    Val60 = 0x60,
    Val61 = 0x61,
    Val62 = 0x62,
    Val63 = 0x63,
    Val64 = 0x64,
    Val65 = 0x65,
    Val66 = 0x66,
    Val67 = 0x67,
    Val68 = 0x68,
    Val69 = 0x69,
    Val6A = 0x6a,
    Val6B = 0x6b,
    Val6C = 0x6c,
    Val6D = 0x6d,
    Val6E = 0x6e,
    Val6F = 0x6f,
    Val70 = 0x70,
    Val71 = 0x71,
    Val72 = 0x72,
    Val73 = 0x73,
    Val74 = 0x74,
    Val75 = 0x75,
    Val76 = 0x76,
    Val77 = 0x77,
    Val78 = 0x78,
    Val79 = 0x79,
    Val7A = 0x7a,
    Val7B = 0x7b,
    Val7C = 0x7c,
    Val7D = 0x7d,
    Val7E = 0x7e,
    Val7F = 0x7f,
}
