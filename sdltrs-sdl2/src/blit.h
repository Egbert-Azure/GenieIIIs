#ifndef _BLIT_H
#define _BLIT_H

extern void TrsSoftBlit(SDL_Surface *src, SDL_Rect *srcrect,
                        SDL_Surface *dst, SDL_Rect *dstrect, int xor);
extern void TrsBlitMap(SDL_Palette *src, SDL_PixelFormat *dst);

#endif /* _BLIT_H */
