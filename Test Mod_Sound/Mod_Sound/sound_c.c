#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "fmod.h"
#include "fmod_errors.h"

#include <alloc.h>
#include <memory.h>
#include <mlvalues.h>

FMOD_SYSTEM *fsystem;


typedef struct FSound
{
	FMOD_SOUND *sound;
	FMOD_CHANNEL *channel;
} FSound;

#define ValOfPointer(x)	( x==0 ? (Val_int(0)) : ((value)(x)) )
#define FSoundOfVal(x)	((FSound*)(x))

#define SoundOf(x) 	(x)->sound
#define ChannelOf(x) 	(x)->channel
	

FSound* LoadFSound(const char adr[]);
FSound* LoadFStream(const char adr[]);
int DeleteFSound(FSound *mus);
int PlayFSound(FSound* mus);
int StopFSound(FSound *mus);
int InitFMOD(void);
int QuitFMOD(void);
void Sleep(int ms);
int GetFSoundLength(FSound *mus);
int GetFSoundStatus(FSound *mus);


void Sleep(int ms)
{
	usleep(ms*1000);
}


FSound* LoadFSound(const char adr[])
{
	FSound *mus;
	int r;

	if (! (mus = malloc(sizeof(FSound))) )
	{
		printf("Error while loading new sound : Unable to allocate memory space\n");
		return NULL;
	}

	SoundOf(mus) = NULL;
	ChannelOf(mus) = NULL;

	r = FMOD_System_CreateSound(fsystem, adr, FMOD_CREATESAMPLE, NULL, &(SoundOf(mus)));
	
	if (r != FMOD_OK || !SoundOf(mus))
	{
		free(mus);
		printf("LoadFSound - Error while loading file : \"%s\"\n\t%s\n", adr, FMOD_ErrorString(r));
		return NULL;
	}
	
	FMOD_Sound_SetMode(SoundOf(mus), FMOD_LOOP_OFF);
	return mus;
}

FSound* LoadFStream(const char adr[])
{
	FSound *mus;
	int r;
	

	if (! (mus = malloc(sizeof(FSound))) )
	{
		printf("Error while loading new stream : Unable to allocate memory space\n");
		return NULL;
	}

	SoundOf(mus) = NULL;
	ChannelOf(mus) = NULL;

	r = FMOD_System_CreateSound(fsystem, adr, FMOD_CREATESTREAM | FMOD_ACCURATETIME, NULL, &(SoundOf(mus)));
	
	if (r != FMOD_OK || !SoundOf(mus))
	{
		free(mus);
		printf("LoadFStream - Error while loading file : \"%s\"\n\t%s\n", adr, FMOD_ErrorString(r));
		return NULL;
	}
	
	FMOD_Sound_SetMode(SoundOf(mus), FMOD_LOOP_OFF);
	return mus;
}

int GetFSoundLength(FSound *mus)
{
	int length, r;

	if (!mus || !SoundOf(mus))
	{
		printf("Warning : tried to get length of void sound\n");
		return 0;
	}

	r = FMOD_Sound_GetLength(SoundOf(mus), &length, FMOD_TIMEUNIT_MS);
	if (r != FMOD_OK)
	{
		printf("Error while retrieving time of sound %x\n\t%s\n", (int)SoundOf(mus), FMOD_ErrorString(r));
		return -1;
	}
	
	return length;
}

int GetFSoundStatus(FSound *mus)
{
	FMOD_BOOL playing, paused;
	int r;

	if (!mus || !SoundOf(mus))
	{
		printf("Warning : tried to get status of void sound\n");
		return -1;
	}

	if (!ChannelOf(mus))
		return 0;
	
	r = FMOD_Channel_IsPlaying(ChannelOf(mus), &playing);
	if (r != FMOD_OK)
	{
		printf("Error while retrieving playing state of channel %x\n\t%s\n", (int)ChannelOf(mus), FMOD_ErrorString(r));
		return -1;
	}

	if (playing)
		return 1;

	r = FMOD_Channel_GetPaused(ChannelOf(mus), &paused);
	if (r != FMOD_OK)
	{
		printf("Error while retrieving paused state of channel %x\n\t%s\n", (int)ChannelOf(mus), FMOD_ErrorString(r));
		return -1;
	}

	if (paused)
		return 2;

	return 0;
}

	

int DeleteFSound(FSound *mus)
{
	int r;
	if (!mus || !SoundOf(mus) || !ChannelOf(mus))
	{
		printf("Warning : tried to free void sound");
		return 0;
	}

	FMOD_Channel_Stop(ChannelOf(mus));
	if ((r=FMOD_Sound_Release(SoundOf(mus))) != FMOD_OK)
	{
		printf("FreeFSound - Error while deleting sound %x\n\t%s\n", (int)(SoundOf(mus)), FMOD_ErrorString(r)); 
		return 0;
	}
	
	free(mus);
	return 1;
}
	
	
int PlayFSound(FSound* mus)
{
    	int r;
	if (!mus || !SoundOf(mus))
        {
		printf("Warning : tried to play void sound");
		return 0;
	}
	else
	{
		if (GetFSoundStatus(mus) != 1)
		{
			if ((r=FMOD_System_PlaySound(fsystem, FMOD_CHANNEL_FREE, SoundOf(mus), 0, &(ChannelOf(mus)))) == FMOD_OK)
				return 1;
		}
		else return 1;
	}
	
	printf("PlayFSound - Error while playing sound %x\n\t%s\n", (int)(SoundOf(mus)), FMOD_ErrorString(r));
	return 0;
}

int StopFSound(FSound *mus)
{
	int r;
	if (!mus || !ChannelOf(mus))
	{
		printf("Warning : tried to stop void sound");
		return 0;
	}
	else if ( (r=FMOD_Channel_Stop(ChannelOf(mus))) == FMOD_OK)
		return 1;

	printf("StopFSound - Error while stopping channel %x\n\t%s\n", (int)(ChannelOf(mus)), FMOD_ErrorString(r));
	return 0;
}

int InitFMOD(void)
{
	int r;
	if ( (r=FMOD_System_Create(&fsystem)) != FMOD_OK)
	{
		printf("InitFMOD - Error while creating system\n\t%s\n", FMOD_ErrorString(r));
		return 0;
	}
	else if ( (r=FMOD_System_Init(fsystem, 32, FMOD_INIT_NORMAL, 0)) != FMOD_OK)
	{
		printf("InitFMOD- Error while initializing system\n\t%s\n", FMOD_ErrorString(r));
		return 0;
	}
	
	return 1;
}

int QuitFMOD(void)
{
	int r;
	if ( (r=FMOD_System_Close(fsystem)) != FMOD_OK)
	{
		printf("QuitFMOD - Error while closing system\n\t%s\n", FMOD_ErrorString(r));
		return 0;
	}
	else if ( (r=FMOD_System_Release(fsystem)) != FMOD_OK)
	{
		printf("QuitFMOD - Error while releasing system\n\t%s\n", FMOD_ErrorString(r));
		return 0;
	}
	
	return 1;
}
	


//Envelopes for CamL
value LoadFSound_Env(value adr)
{
	return ValOfPointer( LoadFSound(String_val(adr)) );
}

value LoadFStream_Env(value adr)
{
	return ValOfPointer( LoadFStream(String_val(adr)) );
}

value DeleteFSound_Env(value p)
{
	return Val_int( DeleteFSound( FSoundOfVal(p) ) );
}

value GetFSoundLength_Env(value p)
{
	return Val_int( GetFSoundLength( FSoundOfVal(p) ) );
}

value GetFSoundStatus_Env(value p)
{
	return Val_int( GetFSoundStatus( FSoundOfVal(p) ) );
}

value PlayFSound_Env(value p)
{
    	return Val_int( PlayFSound( FSoundOfVal(p) ) );
}

value StopFSound_Env(value p)
{
	return Val_int( StopFSound( FSoundOfVal(p) ) );
}

value InitFMOD_Env(unit)
{
	return Val_int( InitFMOD() );
}

value QuitFMOD_Env(unit)
{
	return Val_int( QuitFMOD() );
}

value FSoundSpectrum_Env(value p)
{
	float sp[512];
	FSound *mus = FSoundOfVal(p);
	int i;
	value tab = alloc_shr(512, 0);

	if (FMOD_Channel_GetSpectrum(ChannelOf(mus), sp, 512, 0.5, FMOD_DSP_FFT_WINDOW_RECT) != FMOD_OK)
	{
		for (i=0 ; i<512 ; i++)
			sp[i] = 0;
	}

	for (i=0 ; i<512 ; i++)
		initialize(&Field(tab, i), copy_double(sp[i]));

	return tab;
}

value Sleep_Env(value ms)
{
	Sleep(Int_val(ms));
	return Val_int(1);
}
	

	
	







