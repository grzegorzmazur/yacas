#include "yacas/patcher.h"

#include "yacas/lisperror.h"
#include "yacas/lispio.h"
#include "yacas/standard.h"

#include <algorithm>

/** PatchLoad: patch a string, and write to current output.
 *  Everything between <? and ?> is evaluated. The result
 *  is thrown away.
 */
void PatchLoad(const std::string& content,
               std::ostream& out,
               LispEnvironment& env)
{
    std::size_t i = 0;

    for (;;) {
        const std::size_t p = content.find("<?", i);

        const bool found_start_marker = (p != std::string::npos);

        out << content.substr(i, std::min(p, content.length()) - i);

        if (!found_start_marker)
            break;

        std::size_t q = content.find("?>", p + 2);

        if (q == std::string::npos)
            throw LispErrGeneric("closing tag not found when patching");

        InputStatus oldstatus = env.iInputStatus;
        env.iInputStatus.SetTo("String");

        StringInput newInput(content.substr(p + 2, q - p - 2),
                             env.iInputStatus);
        LispLocalInput localInput(env, &newInput);

        DoInternalLoad(env, &newInput);

        env.iInputStatus.RestoreFrom(oldstatus);

        i = q + 2;
    }
}
