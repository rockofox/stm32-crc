# stm32-crc

`stm32-crc` writes a CRC32 (STM32-flavored) checksum to the last 4 bytes of a .bin file.

## Why?
I was reverse engineering the firmware of an STM32-based device and found out that its firmware was checksummed by using the last 4 bytes of the .bin file for a CRC32 checksum. But using a CRC32 algorithm that is quite a bit different from the standard one (see [here](https://stackoverflow.com/a/39683314/1165357)).

## Do I need this?
Probably not. I'm not even sure if this is done across most STM32 controllers or if it's just this particular one that has a special bootloader.
But if this is useful for you, that's great ^^

## Usage

Run the `stm32-crc` executable with one of the following subcommands:

1. **Update**: Update the checksum of a file.

    ```bash
    stm32-crc update firmware.bin
    ```

2. **Check**: Verify if the checksum in a file is valid.

    ```bash
    stm32-crc check firmware.bin
    ```

3. **Calculate**: Calculate and print the checksum of a file to stdout.

    ```bash
    stm32-crc calculate firmware.bin
    ```
