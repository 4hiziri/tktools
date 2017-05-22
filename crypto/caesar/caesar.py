def rot_char(char, n):
    if char.islower():
        max = ord('z')
    else:
        max = ord('Z')

    r = ord(char) + n
    if r > max:
        r -= 26

    return chr(r)


def rot(string, n):
    rot_num = n % 26
    ret = ''

    for c in string:
        if c.isdigit() or not c.isalpha():
            ret += c
        else:
            ret += rot_char(c, rot_num)

    return ret


if __name__ == '__main__':
    import sys
    if len(sys.argv) == 3:
        string = sys.argv[1]
        num = int(sys.argv[2])
        print(rot(string, num))
    else:
        print("argc error")
