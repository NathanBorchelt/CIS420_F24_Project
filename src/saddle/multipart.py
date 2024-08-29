#    Copyright (C) 2013, 2014 Konstantin S. Solnushkin
#
#    This file is part of "SADDLE", a scripting language used to design
#    high-performance computer clusters.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


# If this file is called directly (not imported as a module):
if __name__ == "__main__":
    exit()

import mimetypes
import random
import string

_BOUNDARY_CHARS = string.digits + string.ascii_letters

def encode_multipart(fields, files, boundary=None):
    """Encode a _dictionary_ of form fields and a _list_ of files as multipart/form-data.
    Return a tuple of (body_string, headers_dict).
    "files" is a list of tuples (name, value): "name" is the name used for this file in the
    HTML form (say, "imgfile"), and "value" is a dictionary with required keys 'filename' (say, "1.jpg")
    and 'content' (the byte string with the contents of "1.jpg"), and an optional 'mimetype' (if not specified,
    tries to guess mime type or uses 'application/octet-stream').
    
    Original version was written by Ben Hoyt
    (see http://code.activestate.com/recipes/578668-encode-multipart-form-data-for-uploading-files-via/)
    and licensed under the MIT license.
    
    Rewritten by Konstantin S. Solnushkin for Python ver. 3.
    
    (Below is the unit test which is probably broken)

    >>> body, headers = encode_multipart({'FIELD': 'VALUE'},
    ...                                  [('FILE', {'filename': 'F.TXT', 'content': b'CONTENT'})],
    ...                                  boundary='BOUNDARY')
    >>> print('\n'.join(repr(l) for l in bytes.decode(bytes(body)).splitlines()))
    '--BOUNDARY'
    'Content-Disposition: form-data; name="FIELD"'
    ''
    'VALUE'
    '--BOUNDARY'
    'Content-Disposition: form-data; name="FILE"; filename="F.TXT"'
    'Content-Type: text/plain'
    ''
    'CONTENT--BOUNDARY--'
    
    >>> print(sorted(headers.items()))
    [('Content-Length', '191'), ('Content-Type', 'multipart/form-data; boundary=BOUNDARY')]
    >>> len(body)
    191
    
    Sample usage:
    fields = {'username': 'BOB SMITH'}
    files = [('imgfile', file_request_dict('1.jpg'))]
    (data, headers) = encode_multipart(fields, files)
    url='http://localhost/cgi-bin/imgupload-script.cgi'
    request = urllib.request.Request(url, data=data, headers=headers)
    resp = urllib.request.urlopen(request)
    response = resp.read()
    print('\n'.join(l for l in bytes.decode(bytes(response)).splitlines()))
    
    """
    def escape_quote(s):
        return s.replace('"', '\\"')

    # If no string specified to be used as a boundary, compute a random string
    if boundary is None:
        boundary = ''.join(random.choice(_BOUNDARY_CHARS) for i in range(30))
    
    # Create an empty byte array to be used for output
    body = bytearray()

    # Process the _dictionary_ of form fields
    for name, value in fields.items():
        body.extend(str.encode('--{0}\r\n'.format(boundary)))
        body.extend(str.encode('Content-Disposition: form-data; name="{0}"\r\n\r\n'.format(escape_quote(name))))
        body.extend(str.encode(value+'\r\n'))

    # Process the _list_ of files to be uploaded
    for name, value in files:
        filename = value['filename']
        if 'mimetype' in value:
            mimetype = value['mimetype']
        else:
            mimetype = mimetypes.guess_type(filename)[0] or 'application/octet-stream'
        body.extend(str.encode('--{0}\r\n'.format(boundary)))
        body.extend(str.encode('Content-Disposition: form-data; name="{0}"; filename="{1}"\r\n'.format(escape_quote(name), escape_quote(filename))))
        body.extend(str.encode('Content-Type: {0}\r\n\r\n'.format(mimetype)))
        body.extend(value['content'])

    # Add the final boundary line
    body.extend(str.encode('--{0}--\r\n'.format(boundary)))
    
    # Form HTTP headers
    headers = {
        'Content-Type': 'multipart/form-data; boundary={0}'.format(boundary),
        'Content-Length': str(len(body)),
    }

    # All done!
    return (body, headers)

def file_request_dict(filename):
    """Returns a dictionary ready for use with the "encode_multipart" function."""
    # Hope we were not called with an empty argument
    if len(filename) == 0:
        print(__name__ + ': Please specify the file name.')
        return
    # Open the specified file in binary mode and return its contents, together with the filename
    with open(filename, 'rb') as f:
        return {'filename': filename, 'content': f.read()}
